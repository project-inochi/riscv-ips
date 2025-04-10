package aia

import spinal.core._
import spinal.lib._

abstract class AIARequest(idWidth: Int) extends Bundle {
  val id = UInt(idWidth bits)
  val valid = Bool()

  def prioritize(other: AIARequest): Bool
  def pending(threshold: UInt): Bool
  def dummy(): AIARequest
  def verify(cond: Bool): AIARequest = {
    Mux(cond, this, dummy())
  }
}

abstract class AIAInterruptSource(sourceId: Int) extends Area {
  val id = sourceId
  val ie = RegInit(False)
  val ip = RegInit(False)

  def asRequest(idWidth: Int, targetHart: Int): AIARequest

  def doClaim(): Unit = {
    ip := False
  }

  def doSet(): Unit = {
    ip := True
  }

  def doPendingUpdate(pending: Bool): Unit = {
    when(pending) {
      doSet()
    } otherwise {
      doClaim()
    }
  }

  def doEnable(): Unit = {
    ie := True
  }

  def doDisable(): Unit = {
    ie := False
  }

  def doEnableUpdate(enabled: Bool): Unit = {
    when(enabled) {
      doEnable()
    } otherwise {
      doDisable()
    }
  }
}

object AIAOperator {
  def doClaim(interrupts: Seq[AIAInterruptSource], id: UInt) = new Area {
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          interrupt.doClaim()
        }
      }
    }
  }

  def doSet(interrupts: Seq[AIAInterruptSource], id: UInt) = new Area {
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          interrupt.doSet()
        }
      }
    }
  }

  def enable(interrupts: Seq[AIAInterruptSource], id: UInt) = new Area{
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          interrupt.doEnable()
        }
      }
    }
  }

  def disable(interrupts: Seq[AIAInterruptSource], id: UInt) = new Area{
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          interrupt.doDisable()
        }
      }
    }
  }
}

case class AIAGeneric(interrupts: Seq[AIAInterruptSource], targetHart: Int, guestId: Int = 0) extends Area {
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
