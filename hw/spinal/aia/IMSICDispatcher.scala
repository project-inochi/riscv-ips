package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import aia.APlicSim.hartIds

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
  sources       : SxAIA,
  groupId       : Int,
  groupHartId   : Int,
)

object IMSICDispatcher {
  val interruptFileSize: BigInt = 4096

  def apply(bus: BusSlaveFactory, mapping : IMSICDispatcherMapping)(infos : Seq[IMSICDispatcherInfo]) = new Area {
    import mapping._

    require(interruptFileHartSize == 0 || isPow2(interruptFileHartSize), "interruptFileHartSize should be power of 2")
    require(interruptFileGroupSize == 0 || isPow2(interruptFileGroupSize), "interruptFileGroupSize should be power of 2")
    require(!(interruptFileHartOffset != 0 && interruptFileGroupSize == 0), "Can not auto calcuate interruptFileGroupSize when interruptFileHartOffset != 0")

    val numberGuest = infos.map(_.sources.guestId).max
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
    require((interruptFileHartOffset & intFileTestMask) != 0, "interruptFileHartOffset should not cover any interrupt file")

    for (info <- infos) yield new Area {
      val imsic = IMSIC(info.sources)
      val offset = info.groupId * realIntFileGroupSize + info.groupHartId * realIntFileHartSize + interruptFileHartOffset + info.sources.guestId * interruptFileSize

      imsic.driveFrom(bus, offset)
    }
  }
}
