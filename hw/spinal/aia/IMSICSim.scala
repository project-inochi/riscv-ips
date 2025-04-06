package aia

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import config.Config
import spinal.lib.bus.misc.BusSlaveFactory
import _root_.sim._

object IMSICSim extends App {
  val sourcenum = 8
  val hartnum = 2

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
          addressWidth = 16,
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

		dut.io.ie #= 0x3fff

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

  val io = new Bundle{
    val bus = slave(tilelinkbus)
    val ie = in Bits (sourcenum*hartnum bits)
    val ip = out Bits (sourcenum*hartnum bits)
  }

	val SxAIAs = for (hartId <- hartIds) yield new SxAIA(sourceIds, hartId, 0)
	val infos = for (SxAIA <- SxAIAs) yield new IMSICDispatcherInfo(SxAIA, 0, SxAIA.hartId)

  val factoryGen = new bus.tilelink.SlaveFactory(_, true)
  val factory = factoryGen(io.bus)
  val imsicDispatcher = IMSICDispatcher(factory, mapping)(infos)

  val iebundle = io.ie

  for ((imsic, i) <- imsicDispatcher.imsics.zipWithIndex){
    imsic.imsic.registers.interrupts.map(_.ie).asBits() := iebundle((i+1)*sourcenum-1 downto i*sourcenum)
    io.ip((i+1)*sourcenum-1 downto i*sourcenum) := imsic.imsic.registers.interrupts.map(_.ip).asBits()
  }
}
