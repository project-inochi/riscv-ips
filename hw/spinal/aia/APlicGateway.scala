package aia

import spinal.core._
import spinal.lib._

class APlicGenericGateway(interrupts: Seq[APlicSource], requestGen: (APlicSource, Int) => APlicGenericRequest) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)

  val requests = interrupts.sortBy(_.id).map(requestGen(_, idWidth))

  val resultRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  }))
}

case class APlicDirectGateway(interrupts: Seq[APlicSource], hartId: Int, enable: Bool) extends APlicGenericGateway(interrupts, _.asDirectRequest(_, hartId)) {
  val idelivery = RegInit(False)
  val iforce = RegInit(False)
  val ithreshold = RegInit(U(0x0, 8 bits))

  val iep = resultRequest.pending(ithreshold) && enable
  val bestRequest = resultRequest.verify(iep)

  def doBestClaim() = new Area {
    APlic.doClaim(interrupts, bestRequest.id)
  }
}

case class APlicMSIGateway(interrupts: Seq[APlicSource], enable: Bool) extends APlicGenericGateway(interrupts, _.asMSIRequest(_)) {
  val bestRequest = Flow(APlicMSIRequest(resultRequest.id.getWidth))
  val realRequest = resultRequest.asInstanceOf[APlicMSIRequest]
  val realRequestDelayed = Delay(realRequest, 1)
  bestRequest.valid := resultRequest.pending(0) && enable && realRequestDelayed =/= realRequest
  bestRequest.payload := resultRequest.asInstanceOf[APlicMSIRequest]

  val (requestStream, requestStreamAvailability) = bestRequest.queueWithAvailability(8)

  when (bestRequest.valid && requestStreamAvailability > 0) {
    APlic.doClaim(interrupts, bestRequest.id)
  }
}
