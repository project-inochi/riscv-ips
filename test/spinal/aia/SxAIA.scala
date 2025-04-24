package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import aia._

case class SxAIAInterruptSource(sourceId: Int) extends Area {
  val id = sourceId
  val ie = RegInit(False)
  val ip = RegInit(False)

  ie.simPublic()
  ip.simPublic()
}

case class SxAIA(sourceIds: Seq[Int], hartId: Int, guestId: Int) extends Area {
  val interrupts = for (sourceId <- sourceIds) yield new SxAIAInterruptSource(sourceId)

  def asTilelinkIMSICIInfo() = TilelinkIMSICIInfo(hartId, guestId, interrupts.map(_.id))
}

case class SxAIATrigger(block: SxAIA, triggers: Bits) extends Area {
  for ((interrupt, trigger) <- block.interrupts.zip(triggers.asBools)) {
    when(trigger) {
      interrupt.ip := True
    }
  }
}
