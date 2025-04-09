package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.core

case class IMSICInterruptFile(sourceIds: Seq[Int], hartId: Int, guestId: Int) extends Area {
  val maxSource = (sourceIds ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)

  case class IMSICSource(sourceId: Int) extends Area {
    val id = U(sourceId, idWidth bits)
    val trigger = Bool

    trigger := False
  }

  val sources = for (sourceId <- sourceIds) yield new IMSICSource(sourceId)
  val triggers = sources.map(_.trigger).asBits()

  def driveFrom(bus: BusSlaveFactory, baseAddress: BigInt) = new Area{
    val SETEIPNUM_LE_ADDR = 0x000
    val SETEIPNUM_BE_ADDR = 0x004

    val busWithOffset = new BusSlaveFactoryAddressWrapper(bus, baseAddress)

    /* Main work, mapping the irq set */
    val target = Flow(UInt(idWidth bits))
    target.valid := False
    target.payload.assignDontCare()
    when(target.valid) {
      switch(target.payload) {
        for (source <- sources) {
          is (source.id) {
            source.trigger := True
          }
        }
      }
    }

    val targetDriveLE = busWithOffset.createAndDriveFlow(UInt(32 bits), address = SETEIPNUM_LE_ADDR, documentation = "Set External Interrupt-Pending bit for %s of hart %d by Little-Endian Number".format(if (guestId == 0) "non-guest" else s"guest ${guestId}", hartId))
    when(targetDriveLE.valid) {
      target.valid := True
      /* TODO: LE -> native */
      target.payload := targetDriveLE.payload.resized
    }

    val targetDriveBE = busWithOffset.createAndDriveFlow(UInt(32 bits), address = SETEIPNUM_BE_ADDR, documentation = "Set External Interrupt-Pending bit for %s of hart %d by Big-Endian Number".format(if (guestId == 0) "non-guest" else s"guest ${guestId}", hartId))
    when(targetDriveBE.valid) {
      target.valid := True
      /* TODO: BE -> native */
      target.payload := targetDriveBE.payload.resized
    }
  }
}


/**
 * IMSICMapping: IMSIC interrupt file mapping info
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
case class IMSICMapping(
  interruptFileHartSize       : BigInt = 0,
  interruptFileHartOffset     : BigInt = 0,
  interruptFileGroupSize      : BigInt = 0,
)

case class IMSICInfo(
  hartId        : Int,
  guestId       : Int,
  sourceIds     : Seq[Int],
  groupId       : Int,
  groupHartId   : Int,
)

case class SxAIAInfo(sources: SxAIA, groupId: Int, groupHartId: Int) {
  def asIMSICInfo(): IMSICInfo = IMSICInfo(
    hartId      = sources.hartId,
    guestId     = sources.guestId,
    sourceIds   = sources.interrupts.map(_.id),
    groupId     = groupId,
    groupHartId = groupHartId,
  )
}

object IMSIC {
  val interruptFileSize: BigInt = 4096

  def mappingCalibrate(mapping : IMSICMapping, maxGuestId: Int, maxGroupHartId: Int, maxGroupId: Int): IMSICMapping = {
    import mapping._

    require(interruptFileHartSize == 0 || isPow2(interruptFileHartSize), "interruptFileHartSize should be power of 2")
    require(interruptFileGroupSize == 0 || isPow2(interruptFileGroupSize), "interruptFileGroupSize should be power of 2")
    require(!(interruptFileHartOffset != 0 && interruptFileGroupSize == 0), "Can not auto calcuate interruptFileGroupSize when interruptFileHartOffset != 0")

    require(maxGuestId < 16, "Per hart can only have max 15 guest interrupt files.")

    val intFileNumber = 1 << log2Up(maxGuestId + 1)
    val minIntFileHartSize = interruptFileSize * intFileNumber
    val realIntFileHartSize = if (interruptFileHartSize != 0) interruptFileHartSize else minIntFileHartSize
    require(realIntFileHartSize >= minIntFileHartSize)

    val intFileGroupHarts = 1 << log2Up(maxGroupHartId + 1)
    val minIntFileGroupSize = realIntFileHartSize * intFileGroupHarts
    val realIntFileGroupSize = if (interruptFileGroupSize != 0) interruptFileGroupSize else minIntFileGroupSize
    require(realIntFileGroupSize >= minIntFileGroupSize)

    val intFileGroupMask = minIntFileGroupSize - 1
    val intFileGroupIdMask = 1 << log2Up(maxGroupId + 1) - 1
    val intFileTestMask = intFileGroupMask + realIntFileGroupSize * intFileGroupIdMask
    require((interruptFileHartOffset & intFileTestMask) == 0, "interruptFileHartOffset should not cover any interrupt file")

    return IMSICMapping(
      interruptFileHartSize   = realIntFileHartSize,
      interruptFileHartOffset = interruptFileHartOffset,
      interruptFileGroupSize  = realIntFileGroupSize,
    )
  }

  def apply(bus: BusSlaveFactory, mapping : IMSICMapping)(infos : Seq[IMSICInfo]) = new Area {
    val maxGuestId = infos.map(_.guestId).max
    val maxGroupHartId = infos.map(_.groupHartId).max
    val maxGroupId = infos.map(_.groupId).max

    val realMapping = mappingCalibrate(mapping, maxGuestId, maxGroupHartId, maxGroupId)

    import realMapping._

    val files = for (info <- infos) yield new Area {
      val file = IMSICInterruptFile(info.sourceIds, info.hartId, info.guestId)
      val offset = info.groupId * interruptFileGroupSize + info.groupHartId * interruptFileHartSize + interruptFileHartOffset + info.guestId * interruptFileSize

      file.driveFrom(bus, offset)
    }

    val triggers = Vec(files.map(_.file.triggers))
  }
}
