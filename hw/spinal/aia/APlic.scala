package aia

import spinal.core._
import spinal.lib._

case class APlicSlaveInfo(childIdx: Int, sourceIds: Seq[Int])

case class APlic(sourceIds: Seq[Int], hartIds: Seq[Int], slaveInfos: Seq[APlicSlaveInfo]) extends Area {
  require(sourceIds.distinct.size == sourceIds.size, "APlic requires no duplicate interrupt source")
  require(hartIds.distinct.size == hartIds.size, "APlic requires no duplicate harts")

  val sources = Bits(sourceIds.size bits)
  val directTargets = Bits(hartIds.size bits)
  val slaveSources = Vec(slaveInfos.map(slaveInfo => Bits(slaveInfo.sourceIds.size bits)))

  val slaveInterruptIds = slaveInfos.flatMap(slaveInfo => slaveInfo.sourceIds).distinct
  val interruptDelegatable = for (sourceId <- sourceIds) yield slaveInterruptIds.find(_ == sourceId).isDefined

  val domainEnable = RegInit(False)
  val deliveryMode = RegInit(False)
  val bigEndian = False

  val msiaddrcfg = new Area {
    val lock = RegInit(False)
    val ppn = RegInit(U(0x0, 44 bits))
    val hhxs = RegInit(U(0x0, 5 bits))
    val lhxs = RegInit(U(0x0, 3 bits))
    val hhxw = RegInit(U(0x0, 3 bits))
    val lhxw = RegInit(U(0x0, 4 bits))

    val maskH = U(1) << hhxw - 1
    val maskL = U(1) << lhxw - 1

    val readable = !lock && deliveryMode

    val msiaddrh = U(0x0, 32 bits)
    msiaddrh(31) := lock
    msiaddrh(28 downto 24) := hhxs
    msiaddrh(22 downto 20) := lhxs
    msiaddrh(18 downto 16) := hhxw
    msiaddrh(15 downto 12) := lhxw
    msiaddrh(11 downto 0) := ppn(43 downto 32)

    def msiAddress(hartIndex: UInt, guestIndex: UInt = 0): UInt = {
      val groupId = (hartIndex >> lhxw) & maskH.resized
      val hartId = hartIndex & maskL.resized
      val groupOffset = groupId << (hhxs + 12)
      val hartOffset = hartId << lhxs

      val msiaddr = (ppn | groupOffset.resized | hartOffset.resized | guestIndex) << 12
      msiaddr
    }
  }

  val interrupts: Seq[APlicInterruptSource] = for (((sourceId, delegatable), i) <- sourceIds.zip(interruptDelegatable).zipWithIndex)
    yield new APlicInterruptSource(sourceId, delegatable, APlicDomainState(domainEnable, deliveryMode), sources(i))

  val slaveMappings = for ((slaveInfo, slaveSource) <- slaveInfos.zip(slaveSources)) yield new Area {
    for ((slaveSourceId, idx) <- slaveInfo.sourceIds.zipWithIndex) yield new Area {
      interrupts.find(_.id == slaveSourceId).map(interrupt => new Area {
        when(domainEnable && interrupt.delegated && (Bool(slaveInfos.size == 1) || interrupt.childIdx === slaveInfo.childIdx)) {
          slaveSource(idx) := interrupt.input
        } otherwise {
          slaveSource(idx) := False
        }
      })
    }
  }

  // hartids
  val directGateways = for (hartId <- hartIds) yield new APlicDirectGateway(interrupts, hartId)

  directTargets := Mux(deliveryMode, B(0), directGateways.map(_.output).asBits())
}

object APlic {
  def doWhenMatch(interrupts: Seq[APlicGenericInterruptSource], id: UInt, func: APlicGenericInterruptSource => Unit) = new Area {
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          func(interrupt)
        }
      }
    }
  }

  def doClaim(interrupts: Seq[APlicGenericInterruptSource], id: UInt) = doWhenMatch(interrupts, id, _.doClaim())

  def doSet(interrupts: Seq[APlicGenericInterruptSource], id: UInt) = doWhenMatch(interrupts, id, _.doSet())

  def doEnable(interrupts: Seq[APlicGenericInterruptSource], id: UInt) = doWhenMatch(interrupts, id, _.doEnable())

  def doDisable(interrupts: Seq[APlicGenericInterruptSource], id: UInt) = doWhenMatch(interrupts, id, _.doDisable())
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

// hartIds
case class APlicDirectGateway(interrupts: Seq[APlicInterruptSource], id: Int) extends Bundle {
  val idelivery = RegInit(False)
  val iforce = RegInit(False)
  val ithreshold = RegInit(U(0x0, 8 bits))

  // topi can be found in generic.bestRequest
  val generic = APlicGenericGateways(interrupts, id)
  generic.threshold := ithreshold.resized

  val output = generic.claim > 0
}

case class APlicRequest(idWidth: Int, priorityWidth: Int) extends APlicGenericRequest(idWidth) {
  val prio = UInt(priorityWidth bits)

  override def prioritize(other: APlicGenericRequest): Bool = {
    val x = other.asInstanceOf[APlicRequest]
    !x.valid || (valid && ((prio < x.prio) || ((prio === x.prio) && (id <= x.id))))
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (prio < threshold))
  }

  override def dummy(): APlicGenericRequest = {
    val tmp = APlicRequest(idWidth, priorityWidth)
    tmp.id := 0
    tmp.valid := False
    tmp.prio := 0
    tmp
  }
}

case class APlicInterruptSource(sourceId: Int, delegatable: Boolean, domaieState: APlicDomainState, input: Bool) extends APlicGenericInterruptSource(sourceId) {
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
    import APlicSourceMode._

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
      import APlicSourceMode._

      val gateway = WhenBuilder()

      gateway.when(mode === INACTIVE) {
        ip := False
        ie := False
      }
      gateway.when(mode === EDGE1) {
        when(input.rise()) {
          ip := True
        }
      }
      gateway.when(mode === EDGE0) {
        when(input.fall()) {
          ip := True
        }
      }
      gateway.when(mode === LEVEL1 && !domaieState.isMSI) {
        ip := input
      }
      gateway.when(mode === LEVEL1 && domaieState.isMSI) {
        when(input.rise()) {
          ip := True
        }
      }
      gateway.when(mode === LEVEL0 && !domaieState.isMSI) {
        ip := ~input
      }
      gateway.when(mode === LEVEL0 && domaieState.isMSI) {
        when(input.fall()) {
          ip := True
        }
      }
    }
  }

  override def asRequest(idWidth: Int, targetHart: Int): APlicGenericRequest = {
    val ret = new APlicRequest(idWidth, prio.getWidth)
    ret.id := U(id)
    ret.valid := ip && ie && (target === targetHart)
    ret.prio := prio
    ret
  }

  override def doClaim(): Unit = {
    when(ipState.allowModify) {
      ip := False
    }
  }

  override def doSet(): Unit = {
    when(ipState.allowModify) {
      ip := True
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
