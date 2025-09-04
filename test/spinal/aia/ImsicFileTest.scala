package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink
import spinal.tester.SpinalSimFunSuite
import aia._
import _root_.sim._

object ImsicFileMapping {
  val iepOffset = 0x000
  val setieOffset = 0x800
  val clrieOffset = 0x804
  val setipOffset = 0x808
  val clripOffset = 0x80c
  val claimOffset = 0x810
  val thresholdOffset = 0x814
}

class MappedImsicFile[T <: spinal.core.Data with IMasterSlave](
  sourceIds: Seq[Int],
  busType: HardType[T],
  factoryGen: T => BusSlaveFactory,
) extends Component {
  val file = new ImsicFile(0, 0, sourceIds)
  val interrupts = file.interrupts

  val io = new Bundle {
    val bus = slave(busType())
  }

  val factory = factoryGen(io.bus)
  val logic = drive(factory)

  def drive(bus: BusSlaveFactory) = new Area {
    import ImsicFileMapping._

    def mapNumArea(offset: Int, func: UInt => Unit) = new Area {
      val numFlow = bus.createAndDriveFlow(UInt(32 bits), address = offset)
      when(numFlow.valid) {
        func(numFlow.payload)
      }
    }

    for (int <- file.interrupts) yield new Area {
      val offset = iepOffset + int.id / 4 * 4
      val bitOffset = (int.id % 4) * 8
      bus.read(int.ip, address = offset, bitOffset = bitOffset)
      bus.read(int.ie, address = offset, bitOffset = bitOffset + 1)

      int.trigger := False
    }

    bus.readAndWrite(file.threshold, address = thresholdOffset, bitOffset = 0)
    bus.read(file.identity, address = claimOffset, bitOffset = 0)
    bus.onWrite(address = claimOffset) {
      file.claim(file.identity)
    }

    val setie = mapNumArea(setieOffset, id => {
      switch (id) {
        for (interrupt <- interrupts) {
          is(interrupt.id) {
            interrupt.ie.set()
          }
        }
      }
    })

    val clrie = mapNumArea(clrieOffset, id => {
      switch (id) {
        for (interrupt <- interrupts) {
          is(interrupt.id) {
            interrupt.ie.clear()
          }
        }
      }
    })

    val setip = mapNumArea(setipOffset, id => {
      switch (id) {
        for ((interrupt, idx) <- interrupts.zipWithIndex) {
          is(interrupt.id) {
            file.triggers(idx) := True
          }
        }
      }
    })

    val clrip = mapNumArea(clripOffset, id => {
      switch (id) {
        for (interrupt <- interrupts) {
          is(interrupt.id) {
            interrupt.ip.clear()
          }
        }
      }
    })
  }
}

case class TilelinkImsicFile(sourceIds: Seq[Int], params: tilelink.BusParameter) extends MappedImsicFile(
  sourceIds,
  new tilelink.Bus(params),
  new tilelink.SlaveFactory(_, true)
)

class ImsicFileTest extends SpinalSimFunSuite {
  onlyVerilator()

  val sourceNum = 64
  val sourceIds = for (i <- 1 until sourceNum) yield i

  val tilelinkBusP = tilelink.M2sParameters(
    sourceCount = 1,
    support = tilelink.M2sSupport(
      addressWidth = 32,
      dataWidth = 32,
      transfers = tilelink.M2sTransfers(
        get = tilelink.SizeRange(1, 8),
        putFull = tilelink.SizeRange(1, 8)
      ),
    )
  )

  test("compile") {
    SimConfig.withConfig(config.TestConfig.spinal).compile(
      new TilelinkImsicFile(sourceIds, tilelinkBusP.toNodeParameters().toBusParameter())
    )
  }

  test("set-test-clear") {
    import ImsicFileMapping._
    SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new TilelinkImsicFile(sourceIds, tilelinkBusP.toNodeParameters().toBusParameter())
    ).doSim{dut => {
      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitRisingEdge()

      assert(!BigInt(agent.get(0, iepOffset + 1, 1).data).testBit(0), "Init state should be false")

      agent.putFullData(0, setipOffset, SimUInt32(1))

      assert(BigInt(agent.get(0, iepOffset + 1, 1).data).testBit(0), "IP is not affected by IE")

      agent.putFullData(0, setieOffset, SimUInt32(1))
      assert(BigInt(agent.get(0, claimOffset, 4).data.reverse) == 1, "Only 1 is enabled")

      agent.putFullData(0, setieOffset, SimUInt32(4))
      agent.putFullData(0, setipOffset, SimUInt32(4))
      assert(BigInt(agent.get(0, claimOffset, 4).data.reverse) == 1, "Failed to check claim 1")

      agent.putFullData(0, thresholdOffset, SimUInt32(1))
      assert(BigInt(agent.get(0, claimOffset, 4).data.reverse) == 0, "Failed to check threshold 1")

      agent.putFullData(0, thresholdOffset, SimUInt32(4))
      assert(BigInt(agent.get(0, claimOffset, 4).data.reverse) == 1, "Failed to check threshold 4")

      agent.putFullData(0, claimOffset, SimUInt32(1))
      assert(BigInt(agent.get(0, claimOffset, 4).data.reverse) == 0, "Failed to check claim 4 threshold 4")

      agent.putFullData(0, thresholdOffset, SimUInt32(0))
      assert(BigInt(agent.get(0, claimOffset, 4).data.reverse) == 4, "Failed to check claim 4")

      agent.putFullData(0, clrieOffset, SimUInt32(4))
      assert(BigInt(agent.get(0, claimOffset, 4).data.reverse) == 0, "Failed to check claim 0")
    }}
  }
}
