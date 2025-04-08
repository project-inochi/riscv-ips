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
  val sourcenum = 256
  val hartnum = 64

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
		dut.io.ie(1) #= 0x3fff

		print(agent.putFullData(0, 0x0, SimUInt32(0x1)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x2)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x3)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x4)))
		print(agent.putFullData(0, 0x0, SimUInt32(0x5)))
		print(agent.putFullData(0, 0x1000, SimUInt32(0x4)))
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
    val ie = in Vec(blocks.map(block => Bits(block.interrupts.size bits)))
    val ip = out Vec(blocks.map(block => Bits(block.interrupts.size bits)))
  }

  val imsicDispatcher = TilelinkIMSICDispatcher(infos.map(_.asIMSICDispatcherInfo()), mapping, p)

  for ((trigger, block) <- imsicDispatcher.io.triggers.zip(blocks)) yield new SxAIATrigger(block, trigger)

  Vec(blocks.map(block => block.interrupts.map(_.ie).asBits())) := io.ie
  io.ip := Vec(blocks.map(block => block.interrupts.map(_.ip).asBits()))
  io.bus <> imsicDispatcher.io.bus
}
