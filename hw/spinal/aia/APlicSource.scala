package aia

import spinal.core._
import spinal.lib._
import spinal.lib.com.spi.sim.FlashModel

abstract class APlicGenericRequest(idWidth: Int) extends Bundle {
  val id = UInt(idWidth bits)
  val valid = Bool()

  def prioritize(other: APlicGenericRequest): Bool
  def pending(threshold: UInt): Bool
  def dummy(): APlicGenericRequest
  def verify(cond: Bool): APlicGenericRequest = {
    Mux(cond, this, dummy())
  }
}

/**
 * Trigger mode for interrupt source
 */
object APlicSourceMode extends SpinalEnum {
  val INACTIVE, DETACHED, EDGE1, EDGE0, LEVEL1, LEVEL0 = newElement()
  defaultEncoding = SpinalEnumEncoding("sm")(
    INACTIVE -> 0,
    DETACHED -> 1,
    EDGE1 -> 4,
    EDGE0 -> 5,
    LEVEL1 -> 6,
    LEVEL0 -> 7)
}

case class APlicDirectRequest(idWidth: Int, priorityWidth: Int) extends APlicGenericRequest(idWidth) {
  val prio = UInt(priorityWidth bits)

  override def prioritize(other: APlicGenericRequest): Bool = {
    val x = other.asInstanceOf[APlicDirectRequest]
    val prioCheck = (prio < x.prio) || ((prio === x.prio) && (id <= x.id))

    // !x.valid || (valid && prioCheck)
    !x.valid || (valid && ((x.id === 0) || ((id =/= 0) && prioCheck)))
  }

  override def pending(threshold: UInt): Bool = {
    val prioCheck = (threshold === 0) || (prio < threshold)

    // vaild && prioCheck
    valid && ((id === 0) || ((id =/= 0) && prioCheck ))
  }

  override def dummy(): APlicGenericRequest = {
    val tmp = APlicDirectRequest(idWidth, priorityWidth)
    tmp.id := 0
    tmp.valid := False
    tmp.prio := 0
    tmp
  }
}

case class APlicMSITarget() extends Bundle {
  val hartId = UInt(14 bits)
  val guestId = UInt(6 bits)
  val eiid = UInt(11 bits)
}

case class APlicMSIRequest(idWidth: Int) extends APlicGenericRequest(idWidth) {
  val target = APlicMSITarget()
  val keep = RegNext(valid) init(False)

  override def prioritize(other: APlicGenericRequest): Bool = {
    val x = other.asInstanceOf[APlicMSIRequest]
    !x.valid || (valid && id <= x.id)
  }

  override def pending(threshold: UInt): Bool = {
    valid
  }

  override def dummy(): APlicGenericRequest = {
    val tmp = APlicMSIRequest(idWidth)
    tmp.target.assignDontCare()
    tmp.id := 0
    tmp.valid := False
    tmp
  }
}

case class APlicSource(sourceId: Int, delegatable: Boolean, isMSI: Bool, input: Bool) extends Area {
  import APlicSourceMode._

  val id = sourceId
  val ie = RegInit(False)
  val ip = RegInit(False)
  val config = RegInit(U(0, 11 bits))
  val delegated = config(10)
  val childIdx = config(9 downto 0)
  val modeBit = delegated ? APlicSourceMode.INACTIVE.asBits | config(2 downto 0).asBits
  val mode = APlicSourceMode()

  mode.assignFromBits(modeBit)

  val targetId = RegInit(U(0x0, 14 bits))
  val prio = RegInit(U(1, 8 bits))

  // for msi delivery mode
  val guestId = RegInit(U(0x0, 6 bits))
  val eiid = RegInit(U(0x0, 11 bits))


  val state = new Area {
    val rectified = mode.mux(
      EDGE0    -> input.rise(),
      EDGE1    -> input.fall(),
      LEVEL1   -> input,
      LEVEL0   -> ~input,
      default  -> False
    )

    val allowSet = mode.mux(
      EDGE0    -> True,
      EDGE1    -> True,
      LEVEL1   -> Mux(isMSI, rectified, False),
      LEVEL0   -> Mux(isMSI, rectified, False),
      DETACHED -> True,
      default  -> False
    )

    val allowClear = mode.mux(
      EDGE0    -> True,
      EDGE1    -> True,
      LEVEL1   -> Mux(isMSI, rectified, True),
      LEVEL0   -> Mux(isMSI, rectified, True),
      DETACHED -> True,
      default  -> False
    )
  }

  when(delegated) {
    ie := False
  } otherwise {
    val ctx = WhenBuilder()

    ctx.when(mode === INACTIVE) {
      ip := False
      ie := False
    }
    ctx.when(mode === EDGE1) {
      when(input.rise()) {
        ip := True
      }
    }
    ctx.when(mode === EDGE0) {
      when(input.fall()) {
        ip := True
      }
    }
    ctx.when(mode === LEVEL1 && !isMSI) {
      ip := input
    }
    ctx.when(mode === LEVEL1 && isMSI) {
      when(input.rise()) {
        ip := True
      } elsewhen(!input) {
        ip := False
      }
    }
    ctx.when(mode === LEVEL0 && !isMSI) {
      ip := ~input
    }
    ctx.when(mode === LEVEL0 && isMSI) {
      when(input.fall()) {
        ip := True
      } elsewhen(input) {
        ip := False
      }
    }
  }

  def asDirectRequest(idWidth: Int, targetHart: Int): APlicGenericRequest = {
    val ret = new APlicDirectRequest(idWidth, prio.getWidth)
    val enable = ie && !isMSI
    ret.id := U(id)
    ret.valid := ip && enable && (targetId === targetHart)
    ret.prio := prio
    ret
  }

  def asMSIRequest(idWidth: Int): APlicGenericRequest = {
    val ret = new APlicMSIRequest(idWidth)
    val enable = (ie || ret.keep) && isMSI
    ret.target.hartId := targetId
    ret.target.guestId := guestId
    ret.target.eiid := eiid
    ret.id := U(id)
    ret.valid := ip && enable
    ret
  }

  def doClaim(): Unit = {
    when(state.allowClear) {
      ip := False
    }
  }

  def doSet(): Unit = {
    when(state.allowSet) {
      ip := True
    }
  }

  def doPendingUpdate(pending: Bool): Unit = {
    when(pending) {
      doSet()
    } otherwise {
      doClaim()
    }
  }

  def doEnable(): Unit = {
    ie := True
  }

  def doDisable(): Unit = {
    ie := False
  }

  def doEnableUpdate(enabled: Bool): Unit = {
    when(enabled) {
      doEnable()
    } otherwise {
      doDisable()
    }
  }

  def doConfigIpUpdate(modeB: Bits) {
    val mode = APlicSourceMode()
    val ctx = WhenBuilder()

    mode.assignFromBits(modeB)

    ctx.when(mode === INACTIVE) {
      ip := False
    }
    ctx.when(List(EDGE1, LEVEL1).map(mode === _).orR) {
      ip := input
    }
    ctx.when(List(EDGE0, LEVEL0).map(mode === _).orR) {
      ip := ~input
    }
  }

  def setConfig(payload: UInt): Unit = {
    val _delegated = payload(10)

    when (_delegated) {
      if (delegatable) {
        config := payload
      } else {
        config := 0
      }
    } otherwise {
      val _mode = payload(2 downto 0)

      switch (_mode) {
        for (state <- APlicSourceMode.elements) {
          is(state.asBits.asUInt) {
            config := payload(2 downto 0).resized
            doConfigIpUpdate(state.asBits)
          }
        }

        default {
          config := 0
        }
      }
    }
  }
}
