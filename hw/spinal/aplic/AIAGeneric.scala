package aplic

import spinal.core._
import spinal.lib._

case class AIAInterrupteSource(sourceId: Int) extends Area {
  val id = sourceId
  val ie = Bool
  val ip = RegInit(False)
}

case class AIAGeneric(interrupts: Seq[AIAInterrupteSource]) extends Area {
  val maxSource = (interrupts.map(_.id) ++ Seq(0)).max + 1
  val idWidth = log2Up(maxSource)
  val threshold = UInt(idWidth bits)

  def Request(id : UInt, valid : Bool) = {
    val ret = new Request
    ret.id := id
    ret.valid := valid
    ret
  }

  case class Request() extends Bundle{
    val id = UInt(idWidth bits)
    val valid = Bool()
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
