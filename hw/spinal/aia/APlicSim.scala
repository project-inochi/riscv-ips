package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import config.Config

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

    dut.io.sources #= 0x0

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 4, swapalign(BigInt(0x1))))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 8, swapalign(BigInt(0x1))))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 12, swapalign(BigInt(0x4))))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 16, swapalign(BigInt(0x5))))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 20, swapalign(BigInt(0x6))))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 24, swapalign(BigInt(0x6))))
    print(agent.putFullData(0, aplicmap.sourcecfgOffset + 28, swapalign(BigInt(0x7))))

    print(agent.putFullData(0, aplicmap.setieOffset, swapalign(BigInt(0xffffffff))))

    print(agent.putFullData(0, aplicmap.targetOffset + 4, swapalign(BigInt(0x1))))
    print(agent.putFullData(0, aplicmap.targetOffset + 8, swapalign(BigInt(0x2))))
    print(agent.putFullData(0, aplicmap.targetOffset + 12, swapalign(BigInt(0x3))))
    print(agent.putFullData(0, aplicmap.targetOffset + 16, swapalign(BigInt(0x4))))
    print(agent.putFullData(0, aplicmap.targetOffset + 20, swapalign(BigInt(0x5))))
    print(agent.putFullData(0, aplicmap.targetOffset + 24, swapalign(BigInt(0x6))))
    print(agent.putFullData(0, aplicmap.targetOffset + 28, swapalign(BigInt(0x40007))))

    val setipdata = swapalign((BigInt(0x0)))
    print(agent.putFullData(0, aplicmap.setipOffset, setipdata))

    val domaincfgdata = swapalign(BigInt(0x80000100))
    print(agent.putFullData(0, aplicmap.domaincfgOffset, domaincfgdata))

    // begin
    var setipnumdata = swapalign(BigInt(0x1))
    print(agent.putFullData(0, aplicmap.setipnumOffset, setipnumdata))
    dut.clockDomain.waitRisingEdge(10)

    setipnumdata = swapalign(BigInt(0x2))
    print(agent.putFullData(0, aplicmap.setipnumOffset, setipnumdata))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))
    dut.clockDomain.waitRisingEdge(10)

    print(agent.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.idcGroup + aplicmap.claimiOffset, 4))
    print(agent.get(0, aplicmap.idcOffset + aplicmap.idcGroup + aplicmap.claimiOffset, 4))
    dut.clockDomain.waitRisingEdge(10)
  }

  def swapalign(data : BigInt) : Array[Byte] = {
    val swapdata = Array.fill(4)(0.toByte)
    val tmpdata = data.toByteArray
    val fixedData = Array.fill(4 - tmpdata.length)(0.toByte) ++ tmpdata
    swapdata(3) = fixedData(0)
    swapdata(2) = fixedData(1)
    swapdata(1)  = fixedData(2)
    swapdata(0)   = fixedData(3)
    swapdata
  }
}
