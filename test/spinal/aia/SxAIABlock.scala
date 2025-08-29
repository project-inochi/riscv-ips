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

  def asTilelinkIMSICIInfo() = TilelinkIMSICTriggerInfo(hartId, guestId, interrupts.map(_.id))

  def asIMSICInfo(groupId: Int, groupHartId: Int): IMSICInfo = IMSICInfo(
    hartId      = hartId,
    guestId     = guestId,
    sourceIds   = interrupts.map(_.id),
    groupId     = groupId,
    groupHartId = groupHartId,
  )

  def asIMSICInfo(hartPerGroup: Int = 0): IMSICInfo = {
    val info = if (hartPerGroup == 0) {
      asIMSICInfo(
        groupId     = 0,
        groupHartId = hartId,
      )
    } else {
      asIMSICInfo(
        groupId     = hartId / hartPerGroup,
        groupHartId = hartId % hartPerGroup,
      )
    }

    info
  }
}

case class SxAIABlockTrigger(block: SxAIABlock, triggers: Bits) extends Area {
  for ((interrupt, trigger) <- block.interrupts.zip(triggers.asBools)) {
    when(trigger) {
      interrupt.ip := True
    }
  }
}
