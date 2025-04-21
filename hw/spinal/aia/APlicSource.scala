package aia

import spinal.core._
import spinal.lib._

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

case class APlicDomainState(enable: Bool, isMSI: Bool) extends Bundle

case class APlicDirectRequest(idWidth: Int, priorityWidth: Int) extends APlicGenericRequest(idWidth) {
  val prio = UInt(priorityWidth bits)

  override def prioritize(other: APlicGenericRequest): Bool = {
    val x = other.asInstanceOf[APlicDirectRequest]
    !x.valid || (valid && ((prio < x.prio) || ((prio === x.prio) && (id <= x.id))))
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (prio < threshold))
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
  val hartIdx = UInt(14 bits)
  val guestIdx = UInt(6 bits)
  val eiid = UInt(11 bits)
}

case class APlicMSIRequest(idWidth: Int, target: APlicMSITarget) extends APlicGenericRequest(idWidth) {
  override def prioritize(other: APlicGenericRequest): Bool = {
    val x = other.asInstanceOf[APlicMSIRequest]
    !x.valid || (valid && id <= x.id)
  }

  override def pending(threshold: UInt): Bool = {
    valid
  }

  override def dummy(): APlicGenericRequest = {
    val dummyTarget = APlicMSITarget()
    dummyTarget.assignDontCare()

    val tmp = APlicMSIRequest(idWidth, dummyTarget)
    tmp.id := 0
    tmp.valid := False
    tmp
  }
}

case class APlicSource(sourceId: Int, delegatable: Boolean, domaieState: APlicDomainState, input: Bool) {
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

  val target = RegInit(U(0x0, 14 bits))
  val prio = RegInit(U(0x0, 8 bits))

  // for msi delivery mode
  val guestindex = RegInit(U(0x0, 6 bits))
  val eiid = RegInit(U(0x0, 11 bits))

  val ipState = new Area {
    val allowModify = Bool()
    val ctx = WhenBuilder()

    ctx.when(List(LEVEL1, LEVEL0).map(mode === _).orR && !domaieState.isMSI) {
      allowModify := False
    }

    ctx.when(mode === INACTIVE) {
      allowModify := False
    }

    ctx.otherwise {
      allowModify := !delegated
    }
  }

  when(domaieState.enable) {
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
      ctx.when(mode === LEVEL1 && !domaieState.isMSI) {
        ip := input
      }
      ctx.when(mode === LEVEL1 && domaieState.isMSI) {
        when(input.rise()) {
          ip := True
        }
      }
      ctx.when(mode === LEVEL0 && !domaieState.isMSI) {
        ip := ~input
      }
      ctx.when(mode === LEVEL0 && domaieState.isMSI) {
        when(input.fall()) {
          ip := True
        }
      }
    }
  }

  def asDirectRequest(idWidth: Int, targetHart: Int): APlicGenericRequest = {
    val ret = new APlicDirectRequest(idWidth, prio.getWidth)
    ret.id := U(id)
    ret.valid := ip && ie && (target === targetHart)
    ret.prio := prio
    ret
  }

  def asMSIRequest(idWidth: Int): APlicGenericRequest = {
    val MSITarget = APlicMSITarget()
    MSITarget.hartIdx := target
    MSITarget.guestIdx := guestindex
    MSITarget.eiid := eiid

    val ret = new APlicMSIRequest(idWidth, MSITarget)
    ret.id := U(id)
    ret.valid := ip && ie
    ret
  }

  def doClaim(): Unit = {
    when(ipState.allowModify) {
      ip := False
    }
  }

  def doSet(): Unit = {
    when(ipState.allowModify) {
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
          }
        }

        default {
          config := 0
        }
      }
    }
  }
}
