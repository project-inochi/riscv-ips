package aia

import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import spinal.core.fiber.{Fiber, Lock}
import scala.collection.mutable.ArrayBuffer

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

case class TilelinkAPLICFiber() extends Area {
  val node = bus.tilelink.fabric.Node.slave()
  val lock = Lock()
  var core: TilelinkAplic = null

  val sources = ArrayBuffer[APlicBundle]()
  def addsource(id : Int) = {
    sources.addRet(APlicBundle(id))
  }
  val targets = ArrayBuffer[APlicBundle]()
  def addtarget(id : Int) = {
    targets.addRet(APlicBundle(id))
  }
  val slaves = ArrayBuffer[APlicSlaveInfo]()
  val slaveSources = ArrayBuffer[APlicSlaveBundle]()
  def addSlave(slave : APlicSlaveInfo) = {
    val slaveBundle = APlicSlaveBundle(slave.childIdx, slave.sourceIds.size)
    slaves += slave
    slaveSources += slaveBundle
    slaveBundle
  }

  val thread = Fiber build new Area {
    lock.await()

    node.m2s.supported.load(TilelinkAplic.getTilelinkSupport(node.m2s.proposed))
    node.s2m.none()

    core = TilelinkAplic(sources.map(_.id).toSeq, targets.map(_.id).toSeq, slaves.toSeq, node.bus.p)

    core.io.bus <> node.bus
    core.io.sources := sources.map(_.flag).asBits
    // targets.map(_.flag).asBits := core.io.targets
    (targets, core.io.targets.asBools).zipped.foreach(_.flag := _)

    Vec(slaveSources.toSeq.map(_.flag)) := core.io.slaveSources
	  core.aplic.interrupts.foreach(_.ip.simPublic())
  }
}

case class APlicBundle(idx : Int) extends Area{
  val id = idx
  val flag = Bool()
}

case class APlicSlaveBundle(idx : Int, sourceNum: Int) extends Area{
  val id = idx
  val flag = Bits(sourceNum bits)
}
