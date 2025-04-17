package aia.test

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import aia._

case class SxAIARequest(idWidth: Int) extends Bundle {
  val id = UInt(idWidth bits)
  val valid = Bool()

  def prioritize(other: SxAIARequest): Bool = {
    !other.valid || (valid && id <= other.id)
  }

  def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (id < threshold))
  }

  def dummy(): SxAIARequest = {
    val tmp = new SxAIARequest(idWidth)
    tmp.id := 0
    tmp.valid := False
    tmp
  }

  def verify(cond: Bool): SxAIARequest = {
    Mux(cond, this, dummy())
  }
}

case class SxAIAInterruptSource(sourceId: Int) extends Area {
  val id = sourceId
  val ie = RegInit(False)
  val ip = RegInit(False)

  ie.simPublic()
  ip.simPublic()

  def asRequest(idWidth: Int, targetHart: Int): SxAIARequest = {
    val ret = new SxAIARequest(idWidth)
    ret.id := U(id)
    ret.valid := ip && ie
    ret
  }
}

case class SxAIA(sourceIds: Seq[Int], hartId: Int, guestId: Int) extends Area {
  val interrupts = for (sourceId <- sourceIds) yield new SxAIAInterruptSource(sourceId)

  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)
  val threshold = UInt(idWidth bits)

  val requests = interrupts.sortBy(_.id).map(g => g.asRequest(idWidth, hartId))

  val resultRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  }))

  val iep = resultRequest.pending(threshold)
  val bestRequest = resultRequest.verify(iep)
  val claim = bestRequest.id

  threshold.simPublic()
  iep.simPublic()
  bestRequest.simPublic()
  claim.simPublic()

  def doWhenMatch(interrupts: Seq[SxAIAInterruptSource], id: UInt, func: SxAIAInterruptSource => Unit) = new Area {
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          func(interrupt)
        }
      }
    }
  }

  def claimBest() = claim(bestRequest.id)

  def claim(sourceId: Int) = doWhenMatch(interrupts, sourceId, _.ip := False)

  def set(sourceId: Int) = doWhenMatch(interrupts, sourceId, _.ip := True)

  def enable(sourceId: Int) = doWhenMatch(interrupts, sourceId, _.ie := True)

  def disable(sourceId: Int) = doWhenMatch(interrupts, sourceId, _.ie := False)

  def ipWrite(mask: Bits, postion: Int) = new Area {
    for ((ip, offset) <- mask.asBools.zipWithIndex) {
      interrupts.find(_.id == (postion + offset)).map(_.ip := ip)
    }
  }

  def ipRead(postion: Int, size: Int): Bits = {
    val result = B(size bits, default -> false)

    for (offset <- 0 to size) {
      result(offset) := interrupts.find(_.id == (postion + offset)).map(_.ip).getOrElse(False)
    }

    return result
  }

  def ieWrite(mask: Bits, postion: Int) = new Area {
    for ((ie, offset) <- mask.asBools.zipWithIndex) {
      interrupts.find(_.id == (postion + offset)).map(_.ie := ie)
    }
  }

  def ieRead(postion: Int, size: Int): Bits = {
    val result = B(size bits, default -> false)

    for (offset <- 0 to size) {
      result(offset) := interrupts.find(_.id == (postion + offset)).map(_.ie).getOrElse(False)
    }

    return result
  }

  def asTilelinkIMSICIInfo() = TilelinkIMSICIInfo(hartId, guestId, interrupts.map(_.id))
}

case class SxAIATrigger(block: SxAIA, triggers: Bits) extends Area {
  for ((interrupt, trigger) <- block.interrupts.zip(triggers.asBools)) {
    when(trigger) {
      interrupt.ip := True
    }
  }
}
