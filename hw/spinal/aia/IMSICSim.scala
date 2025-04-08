package aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc._
import config.Config
import _root_.sim._

object IMSICSim extends App {
  val sourcenum = 1024
  val hartnum = 512

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i

  val imsicMapping = new IMSICDispatcherMapping(
    interruptFileHartSize   = 0,
		interruptFileHartOffset = 0,
		interruptFileGroupSize  = 0,
	)

  val compile = Config.sim.compile{
    val imsic = new TilelinkIMSIC(sourceIds, hartIds, imsicMapping,
      tilelink.M2sParameters(
        sourceCount = 1,
        support = tilelink.M2sSupport(
          addressWidth = 32,
          dataWidth = 32,
          transfers = tilelink.M2sTransfers(
            get = tilelink.SizeRange(8),
            putFull = tilelink.SizeRange(8)
          )
        )
      ).toNodeParameters().toBusParameter())
    imsic
  }

	compile.doSim{ dut =>
		dut.clockDomain.forkStimulus(10)

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

		dut.io.ie(0) #= 0x3fff

		print(agent.putFullData(0, 0x0, SimUInt32(0x1)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x2)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x3)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x4)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x5)))
	}
}

case class TilelinkIMSIC(sourceIds : Seq[Int], hartIds : Seq[Int], mapping : IMSICDispatcherMapping, p : bus.tilelink.BusParameter) extends Component{
  val tilelinkbus = new bus.tilelink.Bus(p)
  val sourcenum = sourceIds.size
  val hartnum = hartIds.size

  val blocks = for (hartId <- hartIds) yield new SxAIA(sourceIds, hartId, 0)
  val infos = for (block <- blocks) yield new SxAIADispatcherInfo(block, 0, block.hartId)
  val io = new Bundle{
    val bus = slave(tilelinkbus)
    val ie = in Vec(infos.map(info => Bits(sourceIds.size bits)))
    val ip = out Vec(infos.map(info => Bits(info.asIMSICDispatcherInfo.sourceIds.size bits)))
  }

  val imsicDispatcher = TilelinkIMSICDispatcher(infos.map(_.asIMSICDispatcherInfo()), mapping, p)

  for ((trigger, block) <- imsicDispatcher.io.triggers.zip(blocks)) yield new SxAIATrigger(block, trigger)

  Vec(blocks.map(block => block.interrupts.map(_.ie).asBits())) := io.ie
  io.ip := imsicDispatcher.io.triggers
  io.bus <> imsicDispatcher.io.bus
}


case class TestIMSICFiber(sourceIds : Seq[Int], hartIds : Seq[Int]) extends Component {
  val sourcenum = sourceIds.size
  val hartnum = hartIds.size

  val down = tilelink.fabric.Node.down()
  val m2sParams = tilelink.M2sParameters(
    addressWidth = 32,
    dataWidth = 64,
    masters = List(
      tilelink.M2sAgent(
        name = TestIMSICFiber.this,
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
  val fiber = Fiber build new Area {
    down.m2s forceParameters m2sParams

    down.s2m.supported load tilelink.S2mSupport.none()

    val mappings = spinal.lib.system.tag.MemoryConnection.getMemoryTransfers(down)
    for(mapping <- mappings){
      println(s"- ${mapping.where} -> ${mapping.transfers}")
    }

    down.bus <> io.bus
  }

	val blocks = for (hartId <- hartIds) yield new SxAIA(sourceIds, hartId, 0)

  val peripherals = new Area{
    val access = tilelink.fabric.Node()
    access at 0x10000000 of down

    val dispatcher = TilelinkIMSICDispatcherFiber()
    dispatcher.node at 0x00000000 of access

    for (block <- blocks) {
      dispatcher.addIMSICinfo(block)
    }
  }

  val io = new Bundle{
    val bus = slave(tilelink.Bus(m2sParams.toNodeParameters().toBusParameter()))
    val ie = in Bits(sourcenum * hartnum bits)
    val ip = out Bits(sourcenum * hartnum bits)
  }

  blocks.flatMap(block => block.interrupts.map(_.ie)).asBits() := io.ie
  io.ip := blocks.flatMap(block => block.interrupts.map(_.ip)).asBits()
}

object TestIMSICFiberVerilog {
  def main(args: Array[String]) {
    val sourcenum = 8
    val hartnum = 2

    val sourceIds = 1 until sourcenum
    val hartIds = 0 until hartnum

    SpinalVerilog(Config.spinal)(new TestIMSICFiber(sourceIds, hartIds))
  }
}

object IMSICNodeSim extends App {
  val sourcenum = 8
  val hartnum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i

  val compile = Config.sim.compile{
    val imsic = new TestIMSICFiber(sourceIds, hartIds)
    imsic
  }

	compile.doSim{ dut =>
		dut.clockDomain.forkStimulus(10)

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

		dut.io.ie #= 0x3fff

		print(agent.putFullData(0, 0x10000000, SimUInt32(0x1)))
		print(agent.putFullData(0, 0x10000000, SimUInt32(0x2)))
		print(agent.putFullData(0, 0x10000000, SimUInt32(0x3)))
		print(agent.putFullData(0, 0x10000000, SimUInt32(0x4)))
		print(agent.putFullData(0, 0x10000000, SimUInt32(0x5)))
	}
}
