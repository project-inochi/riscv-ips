package aplic

import spinal.core._
import spinal.lib._

abstract class AIARequest(idWidth: Int) extends Bundle {
  val id = UInt(idWidth bits)
  val valid = Bool()

  def prioritize(other: AIARequest) : Bool
  def pending(threshold: UInt) : Bool
}

abstract class AIAInterruptSource(sourceId: Int) extends Area {
  val id = sourceId
  val ie = Bool
  val ip = RegInit(False)

  def asRequest(idWidth : Int, targetHart: Int) : AIARequest
}

case class IMSICRequest(idWidth : Int) extends AIARequest(idWidth) {
  override def prioritize(other: AIARequest): Bool = {
    val x = other.asInstanceOf[IMSICRequest]
    !x.valid || (valid && id <= x.id)
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (id < threshold))
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

object AIAOperator {
  def doClaim(id: UInt, interrupts : Seq[AIAInterruptSource]) = new Area {
    for (interrupt <- interrupts) {
      when (interrupt.id === id) {
        interrupt.ip := False
      }
    }
  }
}

case class AIAGeneric(interrupts: Seq[AIAInterruptSource], targetHart: Int) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)
  val threshold = UInt(idWidth bits)

  val requests = interrupts.sortBy(_.id).map(g => g.asRequest(idWidth, targetHart))

  val bestRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  }))

  val iep = bestRequest.pending(threshold)
  val claim = iep ? bestRequest.id | 0

  def doBestClaim() = new Area {
    AIAOperator.doClaim(bestRequest.id, interrupts)
  }
}
