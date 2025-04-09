package aia

import spinal.core._
import spinal.lib._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib.bus.misc.BusSlaveFactory
import scala.collection.mutable.ArrayBuffer

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
  val bigEndian = RegInit(False)

  val interrupts: Seq[APlicInterruptSource] = for (((sourceId, delegatable), i) <- sourceIds.zip(interruptDelegatable).zipWithIndex)
    yield new APlicInterruptSource(sourceId, delegatable, domainEnable, sources(i))

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

  directTargets := directGateways.map(_.output).asBits
}

class MappedAplic[T <: spinal.core.Data with IMasterSlave](sourceIds : Seq[Int],
                                                           hartIds : Seq[Int],
                                                           slaveInfos : Seq[APlicSlaveInfo],
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component{
  require(sourceIds.distinct.size == sourceIds.size, "APlic requires no duplicate interrupt source")
  require(hartIds.distinct.size == hartIds.size, "APlic requires no duplicate harts")

  val aplicMap = APlicMapping.aplicMap

  val io = new Bundle {
    val bus = slave(busType())
    val sources = in Bits (sourceIds.size bits)
    val targets = out Bits (hartIds.size bits)
    val slaveSources = out Vec(slaveInfos.map(slaveInfo => Bits(slaveInfo.sourceIds.size bits)))
  }

  val aplic = APlic(sourceIds, hartIds, slaveInfos)

  aplic.sources := io.sources
  io.targets := aplic.directTargets
  io.slaveSources := aplic.slaveSources

  val factory = factoryGen(io.bus)
  val mapping = APlicMapper(factory, aplicMap)(aplic)

  /*TODO:
   * MSI
   */
}

case class TilelinkAplic(sourceIds : Seq[Int], hartIds : Seq[Int], slaveInfos : Seq[APlicSlaveInfo], p : bus.tilelink.BusParameter) extends MappedAplic[bus.tilelink.Bus](
  sourceIds,
  hartIds,
  slaveInfos,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)

object TilelinkAplic{
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )

  def addressWidth = 20
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

// hartIds
case class APlicDirectGateway(interrupts : Seq[APlicInterruptSource], id : Int) extends Bundle{
  val idelivery = RegInit(False)
  val iforce = RegInit(False)
  val ithreshold = RegInit(U(0x0, 8 bits))

  // topi can be found in generic.bestRequest
  val generic = AIAGeneric(interrupts, id)
  generic.threshold := ithreshold.resized

  val output = generic.claim > 0
}

case class APlicRequest(idWidth : Int, priorityWidth: Int) extends AIARequest(idWidth) {
  val prio = UInt(priorityWidth bits)

  override def prioritize(other: AIARequest): Bool = {
    val x = other.asInstanceOf[APlicRequest]
    !x.valid || (valid && ((prio < x.prio) || ((prio === x.prio) && (id <= x.id))))
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (prio < threshold))
  }

  override def dummy(): AIARequest = {
    val tmp = APlicRequest(idWidth, priorityWidth)
    tmp.id := 0
    tmp.valid := False
    tmp.prio := 0
    tmp
  }
}

case class APlicInterruptSource(sourceId : Int, delegatable: Boolean, globalIE : Bool, input: Bool) extends AIAInterruptSource(sourceId) {
  val config = RegInit(U(0, 11 bits))
  val delegated = config(10)
  val childIdx = config(9 downto 0)
  val modeBit = delegated ? APlicSourceMode.INACTIVE.asBits | config(2 downto 0).asBits
  val mode = APlicSourceMode()

  mode.assignFromBits(modeBit)

  val target = RegInit(U(0x0, 14 bits))
  val prio = RegInit(U(0x0, 8 bits))
  val blockip = Bool()

  // for msi delivery mode
  val guestindex = RegInit(U(0x0, 6 bits))
  val eiid = RegInit(U(0x0, 11 bits))

  switch(mode) {
    is (APlicSourceMode.LEVEL1, APlicSourceMode.LEVEL0, APlicSourceMode.INACTIVE) {
      blockip := True
    }
    default {
      blockip := delegated
    }
  }

  when(globalIE) {
    when(delegated) {
      ie := False
    } otherwise {
      switch(mode) {
        is(APlicSourceMode.INACTIVE) {
          ip := False
          ie := False
        }
        is(APlicSourceMode.DETACHED) {
        }
        is(APlicSourceMode.EDGE1) {
          when(input.rise()) {
            ip := True
          }
        }
        is(APlicSourceMode.EDGE0) {
          when(input.fall()) {
            ip := True
          }
        }
        is(APlicSourceMode.LEVEL1) {
            ip := input
        }
        is(APlicSourceMode.LEVEL0) {
            ip := ~input
        }
      }
    }
  }


  override def asRequest(idWidth : Int, targetHart : Int): AIARequest = {
    val ret = new APlicRequest(idWidth, prio.getWidth)
    ret.id := U(id)
    ret.valid := ip && ie && (target === targetHart)
    ret.prio := prio
    ret
  }

  override def doClaim(): Unit = {
    when(blockip === False){
      ip := False
    }
  }

  override def doSet(): Unit = {
    when(blockip === False){
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

case class TilelinkAPLICFiber() extends Area {
  val node = bus.tilelink.fabric.Node.slave()
  val lock = Lock()
  var core: TilelinkAplic = null

  val sources = ArrayBuffer[APlicBundle]()
  def addsource(source : APlicBundle) = {
    sources.addRet(source)
  }
  val targets = ArrayBuffer[APlicBundle]()
  def addtarget(target : APlicBundle) = {
    targets.addRet(target)
  }
  val slaves = ArrayBuffer[APlicSlaveInfo]()
  def addslave(slave : APlicSlaveInfo) = {
    slaves.addRet(slave)
  }
  val slavesource = ArrayBuffer[APlicBundle]()
  def addslavesource(slaveBundle : APlicBundle) = {
    slavesource.addRet(slaveBundle)
  }

  val thread = Fiber build new Area {
    lock.await()

    node.m2s.supported.load(TilelinkAplic.getTilelinkSupport(node.m2s.proposed))
    node.s2m.none()

    core = TilelinkAplic(sources.map(_.id).toSeq, targets.map(_.id).toSeq, slaves.toSeq, node.bus.p)

    core.io.bus <> node.bus
    core.io.sources := sources.map(_.flag).asBits
    (targets.map(_.flag), core.io.targets.asBools).zipped.foreach(_ := _)

    (slavesource.map(_.flag), core.io.slaveSources.asBits.asBools).zipped.foreach(_ := _)
  }
}

case class APlicBundle(idx : Int) extends Area{
  val id = idx
  val flag = Bool()
}
