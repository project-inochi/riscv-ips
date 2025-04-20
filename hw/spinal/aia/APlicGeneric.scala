package aia

import spinal.core._
import spinal.lib._

case class APlicGenericGateways(interrupts: Seq[APlicGenericInterruptSource], targetHart: Int, guestId: Int = 0) extends Area {
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
    APlic.doClaim(interrupts, bestRequest.id)
  }
}
