package aia

import spinal.core._
import spinal.lib._

case class APlicDirectGateway(interrupts: Seq[APlicSource], hartId: Int, enable: Bool) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val priorityWidth = (interrupts.map(_.prio.getWidth)).max
  val idWidth = log2Up(maxSource)

  val idelivery = RegInit(False)
  val iforce = RegInit(False)
  val ithreshold = RegInit(U(0x0, 8 bits))

  val iforceRequest = new APlicDirectRequest(idWidth, priorityWidth)
  iforceRequest.id := 0
  iforceRequest.valid := iforce
  iforceRequest.prio := 0

  val requests = Seq(iforceRequest) ++ interrupts.sortBy(_.id).map(_.asDirectRequest(idWidth, hartId))

  val resultRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  }))

  val iep = resultRequest.pending(ithreshold) && enable
  val bestRequest = resultRequest.verify(iep)

  def doBestClaim() = new Area {
    APlic.doClaim(interrupts, bestRequest.id)
  }
}

case class APlicMSIGateway(interrupts: Seq[APlicSource], enable: Bool) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)

  val requests = interrupts.sortBy(_.id).map(_.asMSIRequest(idWidth))

  val resultRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  }))

  val bestRequest = Flow(APlicMSIRequest(resultRequest.id.getWidth))
  val realRequest = resultRequest.asInstanceOf[APlicMSIRequest]
  val realRequestDelayed = Delay(realRequest, 1)
  bestRequest.valid := resultRequest.pending(0) && enable && realRequestDelayed =/= realRequest
  bestRequest.payload := resultRequest.asInstanceOf[APlicMSIRequest]

  val requestStream = bestRequest.toStream

  when (bestRequest.valid) {
    APlic.doClaim(interrupts, bestRequest.id)
  }
}
