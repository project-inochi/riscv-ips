package aplic

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import aplic.aplicMapper.setip

object aplicSim extends App {

  val sourcenum = 8
  val hartnum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = aplicMapping.aplicMap

  val compile = Config.sim.compile{
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

    dut.io.sources #= 0

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    val sourcecfgdata = swapalign(BigInt(0x6))
    for (i <- 1 until sourcenum){
      print(agent.putFullData(0, aplicmap.sourcecfgOffset + (i << aplicmap.idShift), sourcecfgdata))
    }

    val setiedata = swapalign(BigInt(0xffffffff))
    print(agent.putFullData(0, aplicmap.setieOffset, setiedata))

    // var data = BigInt(0x6).toByteArray
    // var fixedData = Array.fill(4 - data.length)(0.toByte) ++ data

    var targetdata = swapalign(BigInt(0x6))
    print(agent.putFullData(0, aplicmap.targetOffset + 4, targetdata))

    targetdata = swapalign(BigInt(0x5))
    print(agent.putFullData(0, aplicmap.targetOffset + 8, targetdata))

    targetdata = swapalign(BigInt(0x7))
    for (i <- 3 until sourcenum){
      print(agent.putFullData(0, aplicmap.targetOffset + (i << aplicmap.idShift), targetdata))
    }

    val domaincfgdata = swapalign(BigInt(0x80000100))
    print(agent.putFullData(0, aplicmap.domaincfgOffset, domaincfgdata))

    // val setipdata = swapalign((BigInt(0x0)))
    // print(agent.putFullData(0, aplicmap.setipOffset, setipdata))

    var setipnumdata = swapalign(BigInt(0x1))
    print(agent.putFullData(0, aplicmap.setipnumOffset, setipnumdata))
    dut.clockDomain.waitRisingEdge(10)

    setipnumdata = swapalign(BigInt(0x2))
    print(agent.putFullData(0, aplicmap.setipnumOffset, setipnumdata))
    dut.clockDomain.waitRisingEdge(10)
    // dut.io.sources(0) #= true
    // dut.clockDomain.waitRisingEdge(10)
    // dut.io.sources(0) #= false
    // dut.io.sources(1) #= true
    // dut.clockDomain.waitRisingEdge(10)
    // dut.io.sources(1) #= false
    // dut.clockDomain.waitRisingEdge(10)
    // print(agent.get(0, aplicmap.claimiOffset, 4))
    // dut.clockDomain.waitRisingEdge(10)
    // print(agent.get(0, aplicmap.claimiOffset, 4))

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
