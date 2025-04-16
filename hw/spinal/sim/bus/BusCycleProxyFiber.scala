package sim.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink._
import spinal.core.fiber.Fiber

case class TilelinkBusCycleProxyFiber() extends Area {
  val up = fabric.Node.up()
  val down = fabric.Node.down()

  val m2sParams = TilelinkBusCycleProxy.getTilelinkMasterSupport(TilelinkBusCycleProxyFiber.this)

  val fiber = Fiber build new Area {
    down.m2s forceParameters m2sParams
    down.s2m.supported load S2mSupport.none()

    up.m2s.supported.load(TilelinkBusCycleProxy.getTilelinkSlaveSupport(up.m2s.proposed))
    up.s2m.none()

    val core = new TilelinkBusCycleProxy(up.bus.p, down.bus.p)

    core.io.masterBus <> down.bus
    core.io.slaveBus <> up.bus
  }
}
