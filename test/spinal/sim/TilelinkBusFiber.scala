package sim

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc._

case class TilelinkBusFiber() extends Area {
  val node = tilelink.fabric.Node.down()

  val m2sParams = tilelink.M2sParameters(
    addressWidth = 32,
    dataWidth = 64,
    masters = List(
      tilelink.M2sAgent(
        name = TilelinkBusFiber.this,
        mapping = List(
          tilelink.M2sSource(
            id = SizeMapping(0, 4),
            emits = tilelink.M2sTransfers(
              get = tilelink.SizeRange(1, 64),
              putFull = tilelink.SizeRange(1, 64)
            )
          )
        )
      )
    )
  )

  var bus: Option[tilelink.Bus] = None

  val fiber = Fiber build new Area {
    node.m2s forceParameters m2sParams

    node.s2m.supported load tilelink.S2mSupport.none()

    val mappings = spinal.lib.system.tag.MemoryConnection.getMemoryTransfers(node)
    for(mapping <- mappings){
      println(s"- ${mapping.where} -> ${mapping.transfers}")
    }

    bus.map(node.bus <> _)
  }
}
