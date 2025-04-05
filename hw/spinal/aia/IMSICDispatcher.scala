package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import aia.APlicSim.hartIds

case class IMSICDispatcherMapping(
  numberGuest                 : Int,
  interruptFileHartSize       : BigInt = 0,
  interruptFileHartOffset     : BigInt = 0,
  interruptFileGroupSize      : BigInt = 0,
)

object IMSICDispatcherMapping {
  val r1g0 = IMSICDispatcherMapping(numberGuest = 0)
  val r1g1 = IMSICDispatcherMapping(numberGuest = 1)
  val r1g3 = IMSICDispatcherMapping(numberGuest = 3)
  val r1g7 = IMSICDispatcherMapping(numberGuest = 7)
  val r1g15 = IMSICDispatcherMapping(numberGuest = 15)
}

case class IMSICDispatcherInfo(
  sources       : SxAIA,
  groupId       : Int,
  groupHartId   : Int,
)

object IMSICDispatcher {
  val interruptFileSize: BigInt = 4096

  def apply(bus: BusSlaveFactory, mapping : IMSICDispatcherMapping)(infos : Seq[IMSICDispatcherInfo]) = new Area {
    import mapping._

    require(numberGuest < 16, "Per hart can only have max 15 guest interrupt files.")
    require(interruptFileHartSize == 0 || isPow2(interruptFileHartSize), "interruptFileHartSize should be power of 2")
    require(interruptFileGroupSize == 0 || isPow2(interruptFileGroupSize), "interruptFileGroupSize should be power of 2")

    val intFileNumber = 1 << log2Up(infos.map(_.sources.guestId).max + 1)
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
    require((interruptFileHartOffset & intFileTestMask) != 0, "interruptFileHartOffset should not cover any interrupt file")

    for (info <- infos) yield new Area {
      val imsic = IMSIC(info.sources)
      val offset = info.groupId * realIntFileGroupSize + info.groupHartId * realIntFileHartSize + interruptFileHartOffset + info.sources.guestId * interruptFileSize

      imsic.driveFrom(bus, offset)
    }
  }
}
