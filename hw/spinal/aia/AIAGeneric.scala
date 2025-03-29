package aia

import spinal.core._
import spinal.lib._

abstract class AIARequest(idWidth: Int) extends Bundle {
  val id = UInt(idWidth bits)
  val valid = Bool()

  def prioritize(other: AIARequest) : Bool
  def pending(threshold: UInt) : Bool
  def dummy() : AIARequest
  def verify(cond: Bool): AIARequest = {
    Mux(cond, this, dummy())
  }
}

abstract class AIAInterruptSource(sourceId: Int) extends Area {
  val id = sourceId
  val ie = Bool
  val ip = RegInit(False)

  def asRequest(idWidth : Int, targetHart: Int) : AIARequest

  def doClaim(): Unit = {
    ip := False
  }

  def doSet(): Unit = {
    ip := True
  }
}

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

object AIAOperator {
  def doClaim(interrupts : Seq[AIAInterruptSource], id: UInt) = new Area {
    for (interrupt <- interrupts) {
      when (interrupt.id === id) {
        interrupt.doClaim()
      }
    }
  }


  def doSet(interrupts : Seq[APLICInterruptSource], id : UInt){
      for (interrupt <- interrupts) {
        when (interrupt.id === id) {
          interrupt.doClaim()
        }
      }
  }}

case class AIAGeneric(interrupts: Seq[AIAInterruptSource], targetHart: Int) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)
  val threshold = UInt(idWidth bits)

  val requests = interrupts.sortBy(_.id).map(g => g.asRequest(idWidth, targetHart))

  val resultRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  }))

  val iep = resultRequest.pending(threshold)
  val bestRequest = resultRequest.verify(iep)
  val claim = bestRequest.id

  def doBestClaim() = new Area {
    AIAOperator.doClaim(interrupts, bestRequest.id)
  }
}
