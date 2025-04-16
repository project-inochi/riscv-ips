package aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc._
import config.Config
import _root_.sim._
import _root_.sim.bus._

case class TilelinkIMSICFiberTest(sourceIds: Seq[Int], hartIds: Seq[Int]) extends Component {
  val sourcenum = sourceIds.size
  val hartnum = hartIds.size

  val masterBus = TilelinkBusFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val blocks = for (hartId <- hartIds) yield new SxAIA(sourceIds, hartId, 0)

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val cycleProxy = TilelinkBusCycleProxyFiber()
    cycleProxy.up at 0x00000000 of access
    crossBar << cycleProxy.down

    val dispatcher = TilelinkIMSICFiber()
    dispatcher.node at 0x10000000 of access

    for (block <- blocks) {
      dispatcher.addIMSICinfo(block)
    }
  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val ie = in Vec(blocks.map(block => Bits(block.interrupts.size bits)))
    val ip = out Vec(blocks.map(block => Bits(block.interrupts.size bits)))
  }

  masterBus.bus = Some(io.bus)

  Vec(blocks.map(block => block.interrupts.map(_.ie).asBits())) := io.ie
  io.ip := Vec(blocks.map(block => block.interrupts.map(_.ip).asBits()))
}

object IMSICSim extends App {
  val sourcenum = 128
  val hartnum = 16

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i

  val compile = Config.sim.compile {
    val imsic = new TilelinkIMSICFiberTest(sourceIds, hartIds)
    imsic
  }

  compile.doSim{ dut =>
    dut.clockDomain.forkStimulus(10)

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    dut.io.ie(0) #= 0x3fff
    dut.io.ie(1) #= 0x3fff

    print(agent.putFullData(0, 0x10000000, SimUInt32(0x1)))
    print(agent.putFullData(0, 0x10000000, SimUInt32(0x2)))
    print(agent.putFullData(0, 0x10000000, SimUInt32(0x3)))
    print(agent.putFullData(0, 0x10000000, SimUInt32(0x4)))
    print(agent.putFullData(0, 0x10000000, SimUInt32(0x5)))
    print(agent.putFullData(0, 0x10001000, SimUInt32(0x4)))
    print(agent.putFullData(0, 0x10001000, SimUInt32(0x8)))
    print(agent.putFullData(0, 0x10001004, SimUInt32(0x7, BIG)))
  }
}
