package aia

import spinal.core._
import spinal.lib._

case class ImsicFile(sourceIds: Seq[Int]) extends Area {
  val idWidth = log2Up((sourceIds ++ Seq(0)).max + 1)

  val triggers = Bits(sourceIds.size bits)
  val threshold = RegInit(U(0, idWidth bits))

  val interrupts = for ((sourceId, idx) <- sourceIds.zipWithIndex) yield new Area {
    val id = sourceId
    val trigger = triggers(idx)
    val ie = RegInit(False)
    val ip = RegInit(False) setWhen(trigger)
  }

  case class ImsicRequest() extends Bundle {
    val id = UInt(idWidth bits)
    val iep = Bool()
  }

  val requests = interrupts.map{i =>
    val request = ImsicRequest()
    request.id := i.id
    request.iep := i.ie && i.ip
    request
  }

  val result = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = !b.iep || (a.iep && a.id < b.id)
    takeA ? a | b
  }))

  val identity = (result.iep && (threshold === 0 || result.id < threshold)) ? result.id | U(0, idWidth bits)

  def claim(id: UInt) = new Area {
    switch(id) {
      for (i <- interrupts) {
        is (i.id) {
          i.ip.clear()
        }
      }
    }
  }
}
