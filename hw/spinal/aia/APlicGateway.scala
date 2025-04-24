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

  val valid = resultRequest.pending(ithreshold) && enable
  val bestRequest = resultRequest.verify(valid)
  val iep = valid && idelivery && enable

  def doBestClaim() = new Area {
    when (idelivery) {
      APlic.doClaim(interrupts, bestRequest.id)
    }
  }
}

case class APlicMSIGateway(interrupts: Seq[APlicSource], enable: Bool) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)

  val requests = interrupts.sortBy(_.id).map(_.asMSIRequest(idWidth))

  val resultRequest = requests.reduceBalancedTree((a, b) => {
    val takeA = a.prioritize(b)
    takeA ? a | b
  })

  val requestStream = Stream(APlicMSIRequest(resultRequest.id.getWidth))
  val requestStreamValidMask = requestStream.valid

  requestStream.valid   := resultRequest.pending(0) && enable
  requestStream.payload := resultRequest.asInstanceOf[APlicMSIRequest]

  when (requestStream.valid && requestStream.ready) {
    APlic.doClaim(interrupts, requestStream.payload.id)
  }
}

// // reserved for old two cycle trigger
// case class APlicMSIGateway(interrupts: Seq[APlicSource], enable: Bool) extends Area {
//   val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
//   val idWidth = log2Up(maxSource)

//   val requests = interrupts.sortBy(_.id).map(_.asMSIRequest(idWidth))

//   val resultRequest = RegNext(requests.reduceBalancedTree((a, b) => {
//     val takeA = a.prioritize(b)
//     takeA ? a | b
//   }))

//   val realRequest = resultRequest.asInstanceOf[APlicMSIRequest]
//   val realRequestDelayed = Delay(realRequest, 1)

//   val requestStream = Stream(APlicMSIRequest(resultRequest.id.getWidth))

//   val requestMask = RegNext(requestStream.ready)
//   val requestStreamValidMask = requestMask || requestStream.valid

//   requestStream.valid   := resultRequest.pending(0) && enable && !requestMask
//   requestStream.payload := realRequest

//   when (requestStream.ready && requestStream.valid) {
//     APlic.doClaim(interrupts, requestStream.payload.id)
//   }
// }
