package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import aia.APlicSim.hartIds

case class IMSICDispatcherMapping(
  numberGuest                 : Int,
  interruptFileSize           : BigInt,
  interruptFileOffset         : BigInt,
  interruptFileGroupSize      : BigInt,
)

object IMSICDispatcherMapping {
  val r1g0 = IMSICDispatcherMapping(
    numberGuest               = 0,
    interruptFileSize         = 0,
    interruptFileOffset       = 0,
    interruptFileGroupSize    = 0,
  )

  val r1g1 = IMSICDispatcherMapping(
    numberGuest               = 1,
    interruptFileSize         = 0,
    interruptFileOffset       = 0,
    interruptFileGroupSize    = 0,
  )

  val r1g7 = IMSICDispatcherMapping(
    numberGuest               = 7,
    interruptFileSize         = 0,
    interruptFileOffset       = 0,
    interruptFileGroupSize    = 0,
  )

  val r1g15 = IMSICDispatcherMapping(
    numberGuest               = 15,
    interruptFileSize         = 0,
    interruptFileOffset       = 0,
    interruptFileGroupSize    = 0,
  )
}

case class IMSICDispatcherInfo(
  sources       : SxAIA,
  groupId       : Int,
  groupHartId   : Int,
)

object IMSICDispatcher {
  val interuptFileSize: BigInt = 4096

  def apply(bus: BusSlaveFactory, mapping : IMSICDispatcherMapping)(infos : Seq[IMSICDispatcherInfo]) = new Area {
    import mapping._

    require(numberGuest < 16, "Per hart can only have max 15 guest interrupt files.")
    require(interruptFileSize == 0 || isPow2(interruptFileSize), "interruptFileSize should be power of 2")
    require(interruptFileGroupSize == 0 || isPow2(interruptFileGroupSize), "interruptFileGroupSize should be power of 2")

    val realInterruptFileSize = if (interruptFileSize != 0) interruptFileSize else interuptFileSize
    val interruptFileNumber =  1 << log2Up(infos.map(_.sources.guestId).max + 1)
    val interruptFileMask = interruptFileNumber - 1

    val realInterruptFileGroupSize = if (interruptFileGroupSize != 0) interruptFileGroupSize else realInterruptFileSize * interruptFileNumber

    val interruptFileGroupNumber = infos.map(_.groupId).max + 1
    val interruptFileGroupMask = 1 << log2Up(interruptFileGroupNumber) - 1
    val interruptTestMask = interruptFileMask * realInterruptFileSize + interruptFileGroupMask * realInterruptFileGroupSize
    require((interruptTestMask & interruptFileOffset) != 0, "interruptFileOffset should not cover any interrupt file")

    for (info <- infos) yield new Area {
      val imsic = IMSIC(info.sources)
      val offset = info.groupId * interruptFileGroupSize + info.groupHartId * interruptFileGroupSize + interruptFileOffset + info.sources.guestId * realInterruptFileSize

      imsic.driveFrom(bus, offset)
    }
  }
}
