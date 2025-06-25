package iommu

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.misc._

case class DdtNonleafEntryFlags() extends Bundle{
  val V = Bool()
}

case class DdtLevel(width: Int,
                    offset: Int,
                    size: Int,
                    leaf: Boolean) {
  def ddi(deviceId: UInt): UInt = deviceId(range)
  def tableOffset(deviceId: UInt): UInt = ddi(deviceId) * U(size)

  val range = offset + width - 1 downto offset
  val ddiRange = log2Up(size) + width - 1 downto log2Up(size)
  def ppnRange = 53 downto 10
  def phyicalRange = 55 downto 12
}

case class DdtSpec(levels: Seq[DdtLevel],
                   ddiWidth: Seq[Int],
                   ddtMode: Seq[Int])

object DdtMode {
  val Off    : Int = 0
  val Bare   : Int = 1
  val Level1 : Int = 2
  val Level2 : Int = 3
  val Level3 : Int = 4
}

object DdtSpec {
  val base = DdtSpec(
    levels = List(
      DdtLevel(width = 7, offset = 0, size = 32, leaf = true),
      DdtLevel(width = 9, offset = 7, size = 8, leaf = false),
      DdtLevel(width = 8, offset = 16, size = 8, leaf = false),
    ),
    ddiWidth = Seq(7, 16, 24),
    ddtMode = Seq(DdtMode.Level1, DdtMode.Level2, DdtMode.Level3),
  )

  val extended = DdtSpec(
    levels = List(
        DdtLevel(width = 6, offset = 0, size = 64, leaf = true),
        DdtLevel(width = 9, offset = 6, size = 8, leaf = false),
        DdtLevel(width = 9, offset = 15, size = 8, leaf = false),
    ),
    ddiWidth = Seq(6, 15, 24),
    ddtMode = Seq(DdtMode.Level1, DdtMode.Level2, DdtMode.Level3),
  )
}

object IOMMUTranslationMode extends SpinalEnum {
  val Off, Bare, LVL1, LVL2, LVL3 = newElement()
  defaultEncoding = SpinalEnumEncoding("mode")(
    Off -> 0,
    Bare -> 1,
    LVL1 -> 2,
    LVL2 -> 3,
    LVL3 -> 4)
}

case class IOMMUTranslator() extends Area {
  val mode = IOMMUTranslationMode()
  val ppn = U(0x0, 44 bits)
}

case class CacheAccessCMD() extends Bundle {
  val address = UInt(64 bits)
  val size = UInt(8 bits)
}

case class CacheAccessRsp() extends Bundle {
  val data = Bits(64 bits)
  val error = Bool()
}

case class CacheAccess() extends Bundle with IMasterSlave {
  val cmd = Stream(CacheAccessCMD())
  val rsp = Flow(CacheAccessRsp())

  def asMaster(): Unit = {
    slave(cmd)
    master(rsp)
  }
}

object IOMMUTranslationState extends SpinalEnum {
  val OK, ERROR, PASS = newElement()
}

object IOMMUTranslationType extends SpinalEnum {
  val Untranslated, Translated, Translation = newElement()
}

case class TranslationPayload() extends Bundle {
  val address = UInt(64 bits)
  val deviceId = UInt(32 bits)
  val tranType = IOMMUTranslationType()
  val state = IOMMUTranslationState()
}

case class TranslationDDTReq() extends Bundle {
  val trans = TranslationPayload()
}

case class TranslationDDTRsp() extends Bundle {
  val trans = TranslationPayload()
  val ddt = UInt(512 bits)
}

case class DDTWalker(spec: DdtSpec) extends Area {
  val io = new Bundle {
    val access = slave(CacheAccess())
    val input = slave Stream(TranslationDDTReq())
    val output = master Stream(TranslationDDTRsp())
  }

  val ppn = Reg(UInt(44 bits))
  val mode = Reg(UInt(4 bits))
  val tranRsp = Reg(TranslationDDTRsp())
  /* busy */

  io.output.payload := tranRsp

  val ctrl = new Area {
    val address = Reg(UInt(64 bits))
    val size = Reg(UInt(8 bits))
    val rspUnbuffered = io.access.rsp

    def cmd = io.access.cmd
    val rsp = rspUnbuffered.stage()

    cmd.valid := False
    cmd.address := address
    cmd.size := size

    val flags = rsp.data.resized.as(DdtNonleafEntryFlags())
    val exception = !flags.V || rsp.error
  }

  val walker = new StateMachine {
    val IDLE = new State
    val PASS, OFF = new State
    val LOAD, CMD, RSP = List.fill(spec.levels.size)(new State)
    val WAIT = new State

    val busy = !isActive(IDLE)

    setEntry(IDLE)

    io.input.ready := False
    io.output.valid := False

    val levelBase = Reg(U(0, 44 bits))

    IDLE.whenIsActive {
      when (io.input.valid) {
        io.input.ready := True
        levelBase := ppn
        tranRsp.trans := io.input.trans

        switch (io.input.trans.state) {
          is(IOMMUTranslationState.OK) {
            switch(mode) {
              for ((mode, level) <- spec.ddtMode.zipWithIndex) {
                is(mode) {
                  goto(CMD(level))
                }
              }

              is(DdtMode.Bare) {
                goto(PASS)
              }

              default {
                goto(OFF)
              }
            }
          }
          default {
            goto(PASS)
          }
        }
      }
    }

    PASS.whenIsActive {
      tranRsp.trans.state := IOMMUTranslationState.PASS
      goto(WAIT)
    }

    OFF.whenIsActive {
      tranRsp.trans.state := IOMMUTranslationState.ERROR
      goto(WAIT)
    }

    val fetch = for ((level, levelId) <- spec.levels.zipWithIndex) yield new Area {
      LOAD(levelId).whenIsActive {
        ctrl.address(level.phyicalRange) := levelBase
        ctrl.address(level.ddiRange) := level.ddi(tranRsp.trans.deviceId)
        ctrl.size := level.size
        goto(CMD(levelId))
      }

      CMD(levelId).whenIsActive {
        /* Conditions */
        when(True) {
          ctrl.cmd.valid := True
          when(ctrl.cmd.ready) {
            goto(RSP(levelId))
          }
        }
      }

      RSP(levelId).whenIsActive {
        when(ctrl.rsp.valid) {
          levelId match {
            case 0 => doneLogic
            case _ => {
              when (ctrl.exception) {
                doneLogic
              } otherwise {
                levelBase := ctrl.rsp.data(level.ppnRange).asUInt
                goto(LOAD(levelId - 1))
              }
            }
          }
        }
      }

      def doneLogic(): Unit = {
        goto(WAIT)
      }
    }

    WAIT.whenIsActive {
      io.output.valid := True
      when (io.output.ready) {
        goto(IDLE)
      }
    }
  }

  def drive(bus: BusSlaveFactory) = new Area {
    import IOMMUGlobal._

    val ddtp = U(0, 54 bits)
    ddtp(3 downto 0) := mode
    ddtp(53 downto 10) := ppn
    bus.readMultiWord(ddtp, address = MAPPING.ddtpOffset)

    val ddtpFlow = bus.createAndDriveFlow(UInt(54 bits), address = MAPPING.ddtpOffset)
  }
}
