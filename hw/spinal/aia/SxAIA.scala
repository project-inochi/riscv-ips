package aia

import spinal.core._
import spinal.lib._
import spinal.core

case class SxAIARequest(idWidth : Int) extends AIARequest(idWidth) {
  override def prioritize(other: AIARequest): Bool = {
    val x = other.asInstanceOf[SxAIARequest]
    !x.valid || (valid && id <= x.id)
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (id < threshold))
  }

  override def dummy(): AIARequest = {
    val tmp = SxAIARequest(idWidth)
    tmp.id := 0
    tmp.valid := False
    tmp
  }
}

case class SxAIAInterruptSource(sourceId : Int) extends AIAInterruptSource(sourceId) {
  override def asRequest(idWidth : Int, targetHart: Int): AIARequest = {
    val ret = new SxAIARequest(idWidth)
    ret.id := U(id)
    ret.valid := ip && ie
    ret
  }
}

case class SxAIA(sourceIds : Seq[Int], hartId: Int, guestId: Int) extends Area {
  val interrupts = for (sourceId <- sourceIds) yield new SxAIAInterruptSource(sourceId)

  val operator = AIAGeneric(interrupts, hartId, guestId)

  val claim = operator.claim
  val iep = operator.iep

  def claimBest() = new Area {
    operator.doBestClaim()
  }

  def claim(sourceId: Int) = new Area {
    AIAOperator.doClaim(interrupts, sourceId)
  }

  def set(sourceId: Int) = new Area {
    AIAOperator.doSet(interrupts, sourceId)
  }

  def enable(sourceId: Int) = new Area {
    AIAOperator.enable(interrupts, sourceId)
  }

  def disable(sourceId: Int) = new Area {
    AIAOperator.disable(interrupts, sourceId)
  }

  def ipWrite(mask: Bits, postion: Int) = new Area {
    for ((ip, offset) <- mask.asBools.zipWithIndex) {
      interrupts.find(_.id == (postion + offset)).map(_.doPendingUpdate(ip))
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
    for ((ip, offset) <- mask.asBools.zipWithIndex) {
      interrupts.find(_.id == (postion + offset)).map(_.doEnableUpdate(ip))
    }
  }

  def ieRead(postion: Int, size: Int): Bits = {
    val result = B(size bits, default -> false)

    for (offset <- 0 to size) {
      result(offset) := interrupts.find(_.id == (postion + offset)).map(_.ie).getOrElse(False)
    }

    return result
  }
}

case class SxAIATrigger(block: SxAIA, triggers: Bits) extends Area {
  for ((interrupt, trigger) <- block.interrupts.zip(triggers.asBools)) {
    when(trigger) {
      interrupt.doSet()
    }
  }
}
