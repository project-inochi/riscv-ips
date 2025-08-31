package aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import scala.collection.mutable.ArrayBuffer

class MappedImsicTrigger[T <: spinal.core.Data with IMasterSlave](infos: Seq[ImsicInfo],
                                                           mapping: ImsicMapping,
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component {
  val io = new Bundle {
    val bus = slave(busType())
    val triggers = out Vec(infos.map(info => Bits(info.sourceIds.size bits)))
  }

  val factory = factoryGen(io.bus)

  val logic = ImsicTrigger(factory, mapping)(infos)

  io.triggers := logic.triggers
}

case class TilelinkImsicTrigger(infos: Seq[ImsicInfo],
                                mapping: ImsicMapping,
                                p: bus.tilelink.BusParameter) extends MappedImsicTrigger[bus.tilelink.Bus](
  infos,
  mapping,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)

object TilelinkImsicTrigger {
  def getTilelinkSupport(transfers: tilelink.M2sTransfers, addressWidth: Int = 20) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed = tilelink.M2sSupport(
      addressWidth = addressWidth,
      dataWidth = 32,
      transfers = transfers
    )
  )
  def getTilelinkSupport(transfers: tilelink.M2sTransfers, mapping: ImsicMapping, infos: Seq[ImsicInfo]): tilelink.M2sSupport = getTilelinkSupport(transfers, addressWidth(mapping, infos))

  def addressWidth(mapping: ImsicMapping, infos: Seq[ImsicInfo]): Int = {
    val maxGuestId = infos.map(_.guestId).max
    val maxGroupHartId = infos.map(_.groupHartId).max
    val maxGroupId = infos.map(_.groupId).max

    ImsicTrigger.addressWidth(mapping, maxGuestId, maxGroupHartId, maxGroupId)
  }
}

case class TilelinkImsicTriggerInfo(hartId: Int, guestId: Int, sourceIds: Seq[Int])

case class TilelinkImsicTriggerFiber() extends Area {
  val node = bus.tilelink.fabric.Node.slave()
  val lock = Lock()

  var mapping: Option[ImsicMapping] = None

  case class SourceTrigger(info: TilelinkImsicTriggerInfo, groupId: Int, groupHartId: Int) {
    val trigger = Bits(info.sourceIds.size bits)

    def asImsicInfo(): ImsicInfo = ImsicInfo(
      hartId      = info.hartId,
      guestId     = info.guestId,
      sourceIds   = info.sourceIds,
      groupId     = groupId,
      groupHartId = groupHartId,
    )
  }

  var infos = ArrayBuffer[SourceTrigger]()
  def addImsicinfo(info: TilelinkImsicTriggerInfo, groupId: Int, groupHartId: Int) = {
    val source =infos.addRet(SourceTrigger(info, groupId, groupHartId))
    source.trigger
  }
  def addImsicinfo(info: TilelinkImsicTriggerInfo, hartPerGroup: Int = 0) = {
    val source = if(hartPerGroup == 0) {
      infos.addRet(SourceTrigger(info, 0, info.hartId))
    } else {
      infos.addRet(SourceTrigger(info, info.hartId / hartPerGroup, info.hartId % hartPerGroup))
    }

    source.trigger
  }

  val thread = Fiber build new Area {
    lock.await()

    val imsicMapping = mapping.getOrElse(ImsicMapping())
    val imsicInfos = infos.map(_.asImsicInfo()).toSeq

    node.m2s.supported.load(TilelinkImsicTrigger.getTilelinkSupport(node.m2s.proposed.transfers, imsicMapping, imsicInfos))
    node.s2m.none()

    val core = TilelinkImsicTrigger(imsicInfos, imsicMapping, node.bus.p)

    core.io.bus <> node.bus

    for ((info, trigger) <- infos.zip(core.io.triggers)) {
      info.trigger := trigger
    }
  }
}
