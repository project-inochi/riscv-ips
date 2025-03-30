package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import config.Config
import _root_.sim.simTools

object APlicSim extends App {

  val sourcenum = 8
  val hartnum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping.aplicMap

  val compile = config.Config.sim.compile{
    val imsic = new TilelinkAplic(sourceIds, hartIds,
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
      ).toNodeParameters().toBusParameter()
    )

    imsic
  }

  compile.doSim{ dut =>
    dut.clockDomain.forkStimulus(10)

    dut.io.sources #= 0b1000000

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    val prioArray = Seq(simTools.UInt32(0x1),
                        simTools.UInt32(0x1),
                        simTools.UInt32(0x4),
                        simTools.UInt32(0x5),
                        simTools.UInt32(0x6),
                        simTools.UInt32(0x6),
                        simTools.UInt32(0x7))
    for (i <- 1 until sourcenum){
      print(agent.putFullData(0, aplicmap.sourcecfgOffset + (i << aplicmap.idShift), prioArray(i-1)))
    }

    print(agent.putFullData(0, aplicmap.setieOffset, simTools.UInt32(0xffffffff)))

    val targetArray = Seq(simTools.UInt32(0x1),
                          simTools.UInt32(0x2),
                          simTools.UInt32(0x3),
                          simTools.UInt32(0x4),
                          simTools.UInt32(0x5),
                          simTools.UInt32(0x6),
                          simTools.UInt32(0x40007))
    for (i <- 1 until sourcenum){
      print(agent.putFullData(0, aplicmap.targetOffset + (i << aplicmap.idShift), targetArray(i-1)))
    }

    val setipdata = simTools.UInt32((0x0))
    print(agent.putFullData(0, aplicmap.setipOffset, setipdata))

    val domaincfgdata = simTools.UInt32(0x80000100)
    print(agent.putFullData(0, aplicmap.domaincfgOffset, domaincfgdata))

    // set/clripnum
    print(agent.putFullData(0, aplicmap.setipnumOffset, simTools.UInt32(0x1)))
    dut.clockDomain.waitRisingEdge(10)
    print(agent.putFullData(0, aplicmap.setipnumOffset, simTools.UInt32(0x2)))
    dut.clockDomain.waitRisingEdge(10)
    print(agent.putFullData(0, aplicmap.clripnumOffset, simTools.UInt32(0x1)))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))

    // setip and the block of setip(num) when mode is high or low
    print(agent.putFullData(0, aplicmap.setipnumOffset, simTools.UInt32(0x5)))
    print(agent.putFullData(0, aplicmap.setipnumOffset, simTools.UInt32(0x6)))
    print(agent.putFullData(0, aplicmap.setipnumOffset, simTools.UInt32(0x7)))
    print(agent.putFullData(0, aplicmap.setipOffset, simTools.UInt32(0b11111100)))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 20, simTools.UInt32(0x1)))
    print(agent.putFullData(0, aplicmap.setipOffset, simTools.UInt32(0b1110001)))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 20, simTools.UInt32(0x6)))

    // input interupt
    print(agent.putFullData(0, aplicmap.clrienumOffset, simTools.UInt32(0x1)))
    dut.io.sources #= 0b0111111
    dut.clockDomain.waitRisingEdge(10)
    dut.io.sources #= 0b0110111
    print(agent.putFullData(0, aplicmap.setienumOffset, simTools.UInt32(0x1)))
    dut.clockDomain.waitRisingEdge(10)
    for (i <- 0 until 6){
      print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))
    }
      print(agent.get(0, aplicmap.idcOffset + aplicmap.idcGroup +aplicmap.claimiOffset, 4))

    dut.io.sources #= 0b1000000

    //end
    dut.clockDomain.waitRisingEdge(10)
  }
}
