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

  def asRequest(idWidth : Int) : AIARequest
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
  override def asRequest(idWidth : Int): AIARequest = {
    val ret = new IMSICRequest(idWidth)
    ret.id := U(id)
    ret.valid := ip && ie
    ret
  }
}

case class APLICRequest(idWidth : Int, priorityWidth: Int) extends AIARequest(idWidth) {
  val prio = UInt(priorityWidth bits)

  override def prioritize(other: AIARequest): Bool = {
    val x = other.asInstanceOf[APLICRequest]
    !x.valid || (valid && ((prio < x.prio) || ((prio === x.prio) && (id <= x.id))))
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (prio < threshold))
  }
}

case class APLICInterruptSource(sourceId: Int, priorityWidth: Int) extends AIAInterruptSource(sourceId) {
  val prio = UInt(priorityWidth bits)

  override def asRequest(idWidth : Int): AIARequest = {
    val ret = new APLICRequest(idWidth, priorityWidth)
    ret.id := U(id)
    ret.valid := ip && ie
    ret.prio := prio
    ret
  }
}

case class AIAGeneric(interrupts: Seq[AIAInterruptSource]) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)
  val threshold = UInt(idWidth bits)


  val requests = interrupts.sortBy(_.id).map(g => g.asRequest(idWidth))

  val bestRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  }))

  val iep = bestRequest.pending(threshold)
  val claim = iep ? bestRequest.id | 0

  def doClaim(id: UInt) = new Area {
    for (interrupt <- interrupts) {
      when (U(interrupt.id) === id) {
        interrupt.ip := False
      }
    }
  }

  def doBestClaim() = new Area {
    doClaim(bestRequest.id)
  }
}
