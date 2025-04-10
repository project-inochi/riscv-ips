package aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import scala.collection.mutable.ArrayBuffer

class MappedIMSIC[T <: spinal.core.Data with IMasterSlave](infos: Seq[IMSICInfo],
                                                           mapping: IMSICMapping,
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component{
  val io = new Bundle{
    val bus = slave(busType())
    val triggers = out Vec(infos.map(info => Bits(info.sourceIds.size bits)))
  }

  val factory = factoryGen(io.bus)

  val logic = IMSIC(factory, mapping)(infos)

  io.triggers := logic.triggers
}

case class TilelinkIMSIC(infos: Seq[IMSICInfo],
                         mapping: IMSICMapping,
                         p: bus.tilelink.BusParameter) extends MappedIMSIC[bus.tilelink.Bus](
  infos,
  mapping,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)

object TilelinkIMSIC {
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport, addressWidth: Int = 20) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport, mapping: IMSICMapping, infos: Seq[IMSICInfo]): tilelink.M2sSupport = getTilelinkSupport(proposed, addressWidth(mapping, infos))

  def addressWidth(mapping: IMSICMapping, infos: Seq[IMSICInfo]): Int = {
    val maxGuestId = infos.map(_.guestId).max
    val maxGroupHartId = infos.map(_.groupHartId).max
    val maxGroupId = infos.map(_.groupId).max

    IMSIC.addressWidth(mapping, maxGuestId, maxGroupHartId, maxGroupId)
  }
}

case class TilelinkIMSICFiber() extends Area {
  val node = bus.tilelink.fabric.Node.slave()
  val lock = Lock()

  var mapping: Option[IMSICMapping] = None

  var infos = ArrayBuffer[SxAIAInfo]()
  def addIMSICinfo(block: SxAIA, groupId: Int, groupHartId: Int) = {
    infos.addRet(SxAIAInfo(block, groupId, groupHartId))
  }
  def addIMSICinfo(block: SxAIA, hartPerGroup: Int = 0) = {
    if(hartPerGroup == 0) {
      infos.addRet(SxAIAInfo(block, 0, block.hartId))
    } else {
      infos.addRet(SxAIAInfo(block, block.hartId / hartPerGroup, block.hartId % hartPerGroup))
    }
  }

  val thread = Fiber build new Area {
    lock.await()

    val imsicMapping = mapping.getOrElse(IMSICMapping())
    val imsicInfos = infos.map(_.asIMSICInfo()).toSeq

    node.m2s.supported.load(TilelinkIMSIC.getTilelinkSupport(node.m2s.proposed, imsicMapping, imsicInfos))
    node.s2m.none()

    val core = TilelinkIMSIC(imsicInfos, imsicMapping, node.bus.p)

    core.io.bus <> node.bus

    for ((trigger, block) <- core.io.triggers.zip(infos)) yield new SxAIATrigger(block.sources, trigger)
  }
}
