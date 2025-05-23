package aia

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import spinal.lib.misc.plic.InterruptCtrlFiber
import scala.collection.mutable.ArrayBuffer

class MappedAplic[TS <: spinal.core.Data with IMasterSlave,
                  TM <: spinal.core.Data with IMasterSlave](
  sourceIds: Seq[Int],
  hartIds: Seq[Int],
  slaveInfos: Seq[APlicSlaveInfo],
  p: APlicDomainParam,
  slaveType: HardType[TS],
  masterType: HardType[TM],
  factoryGen: TS => BusSlaveFactory,
  msiSenderGen: (TM, Stream[APlicMSIPayload]) => Area
) extends Component {
  require(sourceIds.distinct.size == sourceIds.size, "APlic requires no duplicate interrupt source")
  require(hartIds.distinct.size == hartIds.size, "APlic requires no duplicate harts")

  val io = new Bundle {
    val slaveBus = slave(slaveType())
    val masterBus = p.genParam.withMSI generate master(masterType())
    val sources = in Bits (sourceIds.size bits)
    val mmsiaddrcfg = (if (p.isRoot) out else in) UInt (64 bits)
    val smsiaddrcfg = (if (p.isRoot) out else in) UInt (64 bits)
    val targets = p.genParam.withDirect generate (out Bits (hartIds.size bits))
    val slaveSources = out Vec(slaveInfos.map(slaveInfo => Bits(slaveInfo.sourceIds.size bits)))
  }

  if (p.isRoot && (p.genParam.withMSI || p.genParam._withMSIAddrCfg)) {
    io.mmsiaddrcfg.assignDontCare()
    io.smsiaddrcfg.assignDontCare()
  }

  val aplic = APlic(p, sourceIds, hartIds, slaveInfos, msiSenderGen(io.masterBus, _))

  aplic.sources := io.sources
  if (p.genParam.withDirect) {
    io.targets := aplic.direct.targets
  }
  io.slaveSources := aplic.slaveSources

  if (p.isRoot) {
    io.mmsiaddrcfg := aplic.mmsiaddrcfg
    io.smsiaddrcfg := aplic.smsiaddrcfg
  } else {
    aplic.mmsiaddrcfg := io.mmsiaddrcfg
    aplic.smsiaddrcfg := io.smsiaddrcfg
  }

  val factory = factoryGen(io.slaveBus)
  val mapping = APlicMapper(factory, p)(aplic)
}

case class TilelinkAplic(sourceIds: Seq[Int], hartIds: Seq[Int], slaveInfos: Seq[APlicSlaveInfo],
                         domainParam: APlicDomainParam, slaveParams: tilelink.BusParameter,
                         mastersParams: tilelink.BusParameter)
                         extends MappedAplic(
  sourceIds,
  hartIds,
  slaveInfos,
  domainParam,
  new bus.tilelink.Bus(slaveParams),
  new bus.tilelink.Bus(mastersParams),
  new bus.tilelink.SlaveFactory(_, true),
  new APlicTilelinkMasterHelper(_, _)
)

case class APlicTilelinkMasterHelper(bus: tilelink.Bus, stream: Stream[APlicMSIPayload]) extends Area {
  val out = stream.map(payload => {
    val channelA = cloneOf(bus.a.payloadType)
    channelA.opcode   := tilelink.Opcode.A.PUT_FULL_DATA
    channelA.size     := 2
    channelA.source   := 0
    channelA.address  := payload.address.resized
    channelA.data     := payload.data.asBits.resized
    channelA.debugId  := 0
    channelA.mask     := 0xf
    channelA
  })

  bus.a <-< out
  bus.d.ready := True
}

case class TilelinkAPlicMasterParam(addressWidth: Int, pendingSize: Int)

object TilelinkAplic {
  def getTilelinkSlaveSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = 20,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )

  def getTilelinkMasterSupport(param: TilelinkAPlicMasterParam, name: Nameable) = bus.tilelink.M2sParameters(
    addressWidth = param.addressWidth,
    dataWidth = 32,
    masters = List(
      tilelink.M2sAgent(
        name = name,
        mapping = List(
          tilelink.M2sSource(
            id = SizeMapping(0, param.pendingSize),
            emits = tilelink.M2sTransfers(
              putFull = tilelink.SizeRange(4)
            )
          )
        )
      )
    )
  )
}

case class TilelinkAPLICFiber() extends Area with InterruptCtrlFiber {
  val up = tilelink.fabric.Node.up()
  val down = tilelink.fabric.Node.down()
  val core = Handle[TilelinkAplic]()

  val m2sParams = TilelinkAplic.getTilelinkMasterSupport(TilelinkAPlicMasterParam(64, 4), TilelinkAPLICFiber.this)

  case class SourceSpec(node: InterruptNode, id: Int)
  case class TargetSpec(node: InterruptNode, id: Int)
  case class APlicSlaveBundle(slaveInfo: APlicSlaveInfo) extends Area {
    val flags = slaveInfo.sourceIds.map(_ => InterruptNode.master())
  }

  val sources = ArrayBuffer[SourceSpec]()
  val targets = ArrayBuffer[TargetSpec]()
  var domainParam: Option[APlicDomainParam] = None
  val mmsiaddrcfg = UInt (64 bits)
  val smsiaddrcfg = UInt (64 bits)

  override def createInterruptMaster(id : Int) : InterruptNode = {
    val spec = up.clockDomain on TargetSpec(InterruptNode.master(), id)
    targets += spec
    spec.node
  }

  override def createInterruptSlave(id: Int) : InterruptNode = {
    val spec = up.clockDomain on SourceSpec(InterruptNode.slave(), id)
    sources += spec
    spec.node
  }

  val slaveSources = ArrayBuffer[APlicSlaveBundle]()
  def createInterruptDelegation(slaveInfo: APlicSlaveInfo) = {
    slaveSources.addRet(APlicSlaveBundle(slaveInfo))
  }

  val thread = Fiber build new Area {
    lock.await()

    val p = domainParam.get

    down.m2s forceParameters m2sParams
    down.s2m.supported load tilelink.S2mSupport.none()

    up.m2s.supported.load(TilelinkAplic.getTilelinkSlaveSupport(up.m2s.proposed))
    up.s2m.none()

    val aplic = TilelinkAplic(sources.map(_.id).toSeq, targets.map(_.id).toSeq, slaveSources.map(_.slaveInfo).toSeq, p, up.bus.p, down.bus.p)

    core.load(aplic)

    if (p.genParam.withMSI) {
      core.io.masterBus <> down.bus
    } else {
      down.bus.assignDontCare()
    }

    core.io.slaveBus <> up.bus
    core.io.sources := sources.map(_.node.flag).asBits()
    if (p.genParam.withDirect) {
      Vec(targets.map(_.node.flag)) := core.io.targets.asBools
    }

    if (p.isRoot) {
      mmsiaddrcfg := core.io.mmsiaddrcfg
      smsiaddrcfg := core.io.smsiaddrcfg
    } else {
      core.io.mmsiaddrcfg := mmsiaddrcfg
      core.io.smsiaddrcfg := smsiaddrcfg
    }

    slaveSources.lazyZip(core.io.slaveSources).foreach((slaveSource, ioSlaveSource) => {
      Vec(slaveSource.flags.map(_.flag)) := ioSlaveSource.asBools
    })
  }
}
