package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

case class IMSICRequest(idWidth : Int) extends AIARequest(idWidth) {
  override def prioritize(other: AIARequest): Bool = {
    val x = other.asInstanceOf[IMSICRequest]
    !x.valid || (valid && id <= x.id)
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (id < threshold))
  }

  override def dummy(): AIARequest = {
    val tmp = IMSICRequest(idWidth)
    tmp.id := 0
    tmp.valid := False
    tmp
  }
}

case class IMSICInterruptSource(sourceId : Int) extends AIAInterruptSource(sourceId) {
  override def asRequest(idWidth : Int, targetHart: Int): AIARequest = {
    val ret = new IMSICRequest(idWidth)
    ret.id := U(id)
    ret.valid := ip && ie
    ret
  }
}

case class IMSIC(sources: Seq[IMSICInterruptSource], targetHart: Int) extends Area {
  val maxSource = (sources.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)

  val generic = AIAGeneric(sources, targetHart);

  val claim = UInt(idWidth bits)
  val threshold = UInt(idWidth bits)

  claim := generic.claim
  generic.threshold := threshold

  def driveFrom(bus: BusSlaveFactory) = new Area{
    val SETEIPNUM_LE_ADDR = 0x000
    val SETEIPNUM_BE_ADDR = 0x004

    /* Main work, mapping the irq set */
    val target = Flow(UInt(idWidth bits))
    target.valid := False
    target.payload.assignDontCare()
    when(target.valid) {
      AIAOperator.doSet(sources, target.payload)
    }

    val targetDriveLE = bus.createAndDriveFlow(UInt(32 bits), address = SETEIPNUM_LE_ADDR)
    when(targetDriveLE.valid) {
      target.valid := True
      /* TODO: LE -> native */
      target.payload := targetDriveLE.payload.resized
    }

    val targetDriveBE = bus.createAndDriveFlow(UInt(32 bits), address = SETEIPNUM_BE_ADDR)
    when(targetDriveBE.valid) {
      target.valid := True
      /* TODO: BE -> native */
      target.payload := targetDriveBE.payload.resized
    }
  }
}
