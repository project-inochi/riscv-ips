package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.core

case class IMSIC(sourceIds: Seq[Int]) extends Area {
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

    val targetDriveLE = busWithOffset.createAndDriveFlow(UInt(32 bits), address = SETEIPNUM_LE_ADDR, documentation = "Set External Interrupt-Pending bit by Little-Endian Number")
    when(targetDriveLE.valid) {
      target.valid := True
      /* TODO: LE -> native */
      target.payload := targetDriveLE.payload.resized
    }

    val targetDriveBE = busWithOffset.createAndDriveFlow(UInt(32 bits), address = SETEIPNUM_BE_ADDR, documentation = "Set External Interrupt-Pending bit by Big-Endian Number")
    when(targetDriveBE.valid) {
      target.valid := True
      /* TODO: BE -> native */
      target.payload := targetDriveBE.payload.resized
    }
  }
}
