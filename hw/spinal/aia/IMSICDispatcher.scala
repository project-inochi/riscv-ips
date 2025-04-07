package aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import scala.collection.mutable.ArrayBuffer

/**
 * IMSICDispatcherMapping: IMSIC interrupt file mapping info
 *
 * Each interrupt file address should be calcuated as below:
 * g * 2^E + B + h * 2^D
 *
 * g is the IMSIC group id and the h is the hart id in the group
 *
 * @interruptFileHartSize:
 *   The interrupt file size for one hart, for IMSIC supports guest
 *   interrupt file, this size should cover all guest interrupt
 *   file. This argument is 2^D in the address formula.
 * @interruptFileHartOffset:
 *   The offset of the interrupt file size for one hart. This
 *   argument is B in the address formula.
 * @interruptFileGroupSize:
 *   The group size for one interrupt file group, This argument is
 *   2^E in the address formula.
 *
 * For convenient, all the arguments could be set to zero for auto
 * calcuation. But when `interruptFileHartOffset` is not zero,
 * `interruptFileGroupSize` must be set non-zero, or the calcuation
 * will fail.
 *
 */
case class IMSICDispatcherMapping(
  interruptFileHartSize       : BigInt = 0,
  interruptFileHartOffset     : BigInt = 0,
  interruptFileGroupSize      : BigInt = 0,
)

case class IMSICDispatcherInfo(
  hartId        : Int,
  guestId       : Int,
  sourceIds     : Seq[Int],
  groupId       : Int,
  groupHartId   : Int,
)

case class SxAIADispatcherInfo(sources: SxAIA, groupId: Int, groupHartId: Int) {
  def asIMSICDispatcherInfo(): IMSICDispatcherInfo = IMSICDispatcherInfo(
    hartId      = sources.hartId,
    guestId     = sources.guestId,
    sourceIds   = sources.interrupts.map(_.id),
    groupId     = groupId,
    groupHartId = groupHartId,
  )
}

object IMSICDispatcher {
  val interruptFileSize: BigInt = 4096

  def apply(bus: BusSlaveFactory, mapping : IMSICDispatcherMapping)(infos : Seq[IMSICDispatcherInfo]) = new Area {
    import mapping._

    require(interruptFileHartSize == 0 || isPow2(interruptFileHartSize), "interruptFileHartSize should be power of 2")
    require(interruptFileGroupSize == 0 || isPow2(interruptFileGroupSize), "interruptFileGroupSize should be power of 2")
    require(!(interruptFileHartOffset != 0 && interruptFileGroupSize == 0), "Can not auto calcuate interruptFileGroupSize when interruptFileHartOffset != 0")

    val numberGuest = infos.map(_.guestId).max
    require(numberGuest < 16, "Per hart can only have max 15 guest interrupt files.")

    val intFileNumber = 1 << log2Up(numberGuest + 1)
    val minIntFileHartSize = interruptFileSize * intFileNumber
    val realIntFileHartSize = if (interruptFileHartSize != 0) interruptFileHartSize else minIntFileHartSize
    require(realIntFileHartSize >= minIntFileHartSize)

    val intFileGroupHarts = 1 << log2Up(infos.map(_.groupHartId).max + 1)
    val minIntFileGroupSize = realIntFileHartSize * intFileGroupHarts
    val realIntFileGroupSize = if (interruptFileGroupSize != 0) interruptFileGroupSize else minIntFileGroupSize
    require(realIntFileGroupSize >= minIntFileGroupSize)

    val intFileGroupMask = minIntFileGroupSize - 1
    val intFileGroupIdMask = 1 << log2Up(infos.map(_.groupId).max + 1) - 1
    val intFileTestMask = intFileGroupMask + realIntFileGroupSize * intFileGroupIdMask
    require((interruptFileHartOffset & intFileTestMask) == 0, "interruptFileHartOffset should not cover any interrupt file")

    val imsics = for (info <- infos) yield new Area {
      val imsic = IMSIC(info.sourceIds)
      val offset = info.groupId * realIntFileGroupSize + info.groupHartId * realIntFileHartSize + interruptFileHartOffset + info.guestId * interruptFileSize

      imsic.driveFrom(bus, offset)
    }

    val triggers = Vec(imsics.map(_.imsic.triggers))
  }
}

class MappedIMSICDispatcher[T <: spinal.core.Data with IMasterSlave](infos: Seq[IMSICDispatcherInfo],
                                                                     mapping: IMSICDispatcherMapping,
                                                                     busType: HardType[T],
                                                                     factoryGen: T => BusSlaveFactory) extends Component{
  val io = new Bundle{
    val bus = slave(busType())
    val triggers = out Vec(infos.map(info => Bits(info.sourceIds.size bits)))
  }

  val factory = factoryGen(io.bus)

  val logic = IMSICDispatcher(factory, mapping)(infos)

  io.triggers := logic.triggers
}

case class TilelinkIMSICDispatcher(infos: Seq[IMSICDispatcherInfo],
                                   mapping: IMSICDispatcherMapping,
                                   p: bus.tilelink.BusParameter) extends MappedIMSICDispatcher[bus.tilelink.Bus](
  infos,
  mapping,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)


object TilelinkIMSICDispatcher{
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )

  def addressWidth = 32
}

case class TilelinkIMSICDispatcherFiber() extends Area {
  val node = bus.tilelink.fabric.Node.slave()
  val lock = Lock()

  var infos = ArrayBuffer[SxAIADispatcherInfo]()
  def addIMSICinfo(block: SxAIA, groupId: Int, groupHartId: Int) = {
    infos.addRet(SxAIADispatcherInfo(block, groupId, groupHartId))
  }
  def addIMSICinfo(block: SxAIA, hartPerGroup: Int = 0) = {
    if(hartPerGroup == 0) {
      infos.addRet(SxAIADispatcherInfo(block, 0, block.hartId))
    } else {
      infos.addRet(SxAIADispatcherInfo(block, block.hartId / hartPerGroup, block.hartId % hartPerGroup))
    }
  }

  val thread = Fiber build new Area {
    lock.await()

    node.m2s.supported.load(TilelinkIMSICDispatcher.getTilelinkSupport(node.m2s.proposed))
    node.s2m.none()

    val core = TilelinkIMSICDispatcher(infos.map(_.asIMSICDispatcherInfo()).toSeq, IMSICDispatcherMapping(), node.bus.p)

    core.io.bus <> node.bus

    for ((trigger, block) <- core.io.triggers.zip(infos)) yield new SxAIATrigger(block.sources, trigger)
  }
}
