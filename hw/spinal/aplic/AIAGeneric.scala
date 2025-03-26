package aplic

import spinal.core._
import spinal.lib._

case class AIAInterrupteSource(sourceId: Int, priorityWidth : Int = 0) extends Area {
  val id = sourceId
  val prioWidth = if (priorityWidth > 0) Some(priorityWidth) else None
  val prio = if (priorityWidth > 0) Some(UInt(priorityWidth bits)) else None
  val ie = Bool
  val ip = RegInit(False)
}

case class AIAGeneric(interrupts: Seq[AIAInterrupteSource]) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)
  val threshold = UInt(idWidth bits)
  // val prioWidth = (interrupts.map(_.prioWidth) ++ Seq(0)).max
  val prioWidth = (interrupts.collect { case i if i.prioWidth.isDefined => i.prioWidth.get } ++ Seq(0)).max

  abstract class RequestBase() extends Bundle{
    val id = UInt(idWidth bits)
    val valid = Bool()
  }

  case class Request() extends RequestBase

  case class RequestWithPriority() extends RequestBase{
    val prio = UInt(prioWidth bits)
  }

  if (prioWidth > 0){
    def Request(id : UInt, prio : UInt, valid : Bool) = {
      val ret = new RequestWithPriority
      ret.id := id
      ret.valid := valid
      ret.prio := prio
      ret
    }

    val requests = Request(U(maxSource), Bits(prioWidth bits).setAll().asUInt, True) +: interrupts.sortBy(_.id).map(g =>
      Request(
        id       = U(g.id),
        prio     = g.prio.get,
        valid    = g.ip && g.ie
      )
    )

    val bestRequest = RegNext(requests.reduceBalancedTree((a, b) => {
      val takeA = !b.valid || (a.valid && a.prio <= b.prio)
      takeA ? a | b
    }))

    val iep = (bestRequest.id < maxSource) && ((threshold === 0) || (bestRequest.prio < threshold))
    val claim = iep ? bestRequest.id | 0

    def doClaim(id: UInt) = new Area {
      requests.find(_.id == id).map(_.valid := False)
    }

    def doBestClaim() = new Area {
      doClaim(bestRequest.id)
    }

  }else{
    def Request(id : UInt, valid : Bool) = {
      val ret = new Request
      ret.id := id
      ret.valid := valid
      ret
    }

    val requests = Request(U(maxSource), True) +: interrupts.sortBy(_.id).map(g =>
      Request(
        id       = U(g.id),
        valid    = g.ip && g.ie
      )
    )

    val bestRequest = RegNext(requests.reduceBalancedTree((a, b) => {
      val takeA = !b.valid || (a.valid && a.id <= b.id)
      takeA ? a | b
    }))

    val iep = (bestRequest.id < maxSource) && ((threshold === 0) || (bestRequest.id < threshold))
    val claim = iep ? bestRequest.id | 0

    def doClaim(id: UInt) = new Area {
      requests.find(_.id == id).map(_.valid := False)
    }

    def doBestClaim() = new Area {
      doClaim(bestRequest.id)
    }
  }
}
