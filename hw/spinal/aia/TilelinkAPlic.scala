package aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import scala.collection.mutable.ArrayBuffer

class MappedAplic[T <: spinal.core.Data with IMasterSlave](sourceIds: Seq[Int],
                                                           hartIds: Seq[Int],
                                                           slaveInfos: Seq[APlicSlaveInfo],
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

case class TilelinkAplic(sourceIds: Seq[Int], hartIds: Seq[Int], slaveInfos: Seq[APlicSlaveInfo], p: bus.tilelink.BusParameter) extends MappedAplic[bus.tilelink.Bus](
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
  import scala.collection.{Seq, mutable}

  val node = bus.tilelink.fabric.Node.slave()
  val lock = Lock()
  var core: TilelinkAplic = null

  case class sourceSpec(node: InterruptNode, id: Int)
  case class targetSpec(node: InterruptNode, id: Int)
  case class APlicSlaveBundle(slaveInfo: APlicSlaveInfo) extends Area {
    val flags = slaveInfo.sourceIds.map(_ => InterruptNode.master())
  }

  val sources = ArrayBuffer[sourceSpec]()
  val targets = ArrayBuffer[targetSpec]()

  val mappedInterrupts = mutable.LinkedHashMap[InterruptNode, InterruptNode]()

  def addSource(id: Int, node: InterruptNode) = {
    val spec = sourceSpec(InterruptNode.slave(), id)
    sources += spec
    spec.node << node
    node
  }
  def addTarget(id: Int, node: InterruptNode) = {
    val spec = targetSpec(InterruptNode.master(), id)
    targets += spec
    node << spec.node
    spec.node
  }

  val slaveSources = ArrayBuffer[APlicSlaveBundle]()
  def addSlave(slaveInfo: APlicSlaveInfo) = {
    slaveSources.addRet(APlicSlaveBundle(slaveInfo))
  }

  val thread = Fiber build new Area {
    lock.await()

    node.m2s.supported.load(TilelinkAplic.getTilelinkSupport(node.m2s.proposed))
    node.s2m.none()

    core = TilelinkAplic(sources.map(_.id).toSeq, targets.map(_.id).toSeq, slaveSources.map(_.slaveInfo).toSeq, node.bus.p)

    core.io.bus <> node.bus
    core.io.sources := sources.map(_.node.flag).asBits
    Vec(targets.map(_.node.flag)) := core.io.targets.asBools

    slaveSources.lazyZip(core.io.slaveSources).foreach((slaveSource, ioSlaveSource) => {
      Vec(slaveSource.flags.map(_.flag)) := ioSlaveSource.asBools
    })

	  core.aplic.interrupts.foreach(_.ip.simPublic())
  }
}
