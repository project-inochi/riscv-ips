package aia

import spinal.core._
import spinal.lib._

case class APlicGenericGateway(interrupts: Seq[APlicSource], requestGen: (APlicSource, Int) => APlicGenericRequest) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)
  val threshold = UInt(idWidth bits)

  val requests = interrupts.sortBy(_.id).map(requestGen(_, idWidth))

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

// hartIds
case class APlicDirectGateway(interrupts: Seq[APlicSource], hartId: Int) extends Bundle {
  val idelivery = RegInit(False)
  val iforce = RegInit(False)
  val ithreshold = RegInit(U(0x0, 8 bits))

  // topi can be found in generic.bestRequest
  val generic = APlicGenericGateway(interrupts, _.asRequest(_, hartId))
  generic.threshold := ithreshold.resized

  val output = generic.claim > 0
}
