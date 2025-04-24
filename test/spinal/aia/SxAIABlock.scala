package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import aia._

case class SxAIAInterruptSource(sourceId: Int) extends Area {
  val id = sourceId
  val ie = RegInit(False)
  val ip = RegInit(False)

  val profile = new Area {
    val en = RegInit(False)
    val counter = Counter(32 bits, en && !ip)
    val latency = counter.value

    when (!en) {
      counter.clear()
    }

    en.simPublic()
    latency.simPublic()
  }

  ie.simPublic()
  ip.simPublic()
}

case class SxAIABlock(sourceIds: Seq[Int], hartId: Int, guestId: Int) extends Area {
  val interrupts = for (sourceId <- sourceIds) yield new SxAIAInterruptSource(sourceId)

  def asTilelinkIMSICIInfo() = TilelinkIMSICIInfo(hartId, guestId, interrupts.map(_.id))
}

case class SxAIABlockTrigger(block: SxAIABlock, triggers: Bits) extends Area {
  for ((interrupt, trigger) <- block.interrupts.zip(triggers.asBools)) {
    when(trigger) {
      interrupt.ip := True
    }
  }
}
