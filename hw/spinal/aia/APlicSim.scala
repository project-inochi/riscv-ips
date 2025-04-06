package aia

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import config.Config
import _root_.sim._

object APlicSim extends App {
  val sourcenum = 8
  val hartnum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = IndexedSeq(1, 4)
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping.aplicMap

  val compile = Config.sim.compile{
    val aplics = new aplics()
    aplics
  }

  compile.doSim{ dut =>
    dut.clockDomain.forkStimulus(10)

    dut.io.sources #= 0b1000001

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agentmaster = new tilelink.sim.MasterAgent(dut.io.busmaster, dut.clockDomain)
    val agentslave = new tilelink.sim.MasterAgent(dut.io.busslave, dut.clockDomain)

    val prioArrayM = Seq(SimUInt32(0x400),
                         SimUInt32(0x1),
                         SimUInt32(0x4),
                         SimUInt32(0x400),
                         SimUInt32(0x5),
                         SimUInt32(0x6),
                         SimUInt32(0x7))
    val prioArrayS = Seq(SimUInt32(0x7),
                         SimUInt32(0x0),
                         SimUInt32(0x0),
                         SimUInt32(0x5),
                         SimUInt32(0x0),
                         SimUInt32(0x0),
                         SimUInt32(0x0))
    for (i <- 1 until sourcenum){
      print(agentmaster.putFullData(0, aplicmap.sourcecfgOffset + (i << aplicmap.idShift), prioArrayM(i-1)))
      print(agentslave.putFullData(0, aplicmap.sourcecfgOffset + (i << aplicmap.idShift), prioArrayS(i-1)))
    }

    print(agentmaster.putFullData(0, aplicmap.setieOffset, SimUInt32(0xff)))
    print(agentslave.putFullData(0, aplicmap.setieOffset, SimUInt32(0xff)))

    val targetArray = Seq(SimUInt32(0x1),
                          SimUInt32(0x2),
                          SimUInt32(0x3),
                          SimUInt32(0x4),
                          SimUInt32(0x5),
                          SimUInt32(0x6),
                          SimUInt32(0x40007))
    for (i <- 1 until sourcenum){
      print(agentmaster.putFullData(0, aplicmap.targetOffset + (i << aplicmap.idShift), targetArray(i-1)))
      print(agentslave.putFullData(0, aplicmap.targetOffset + (i << aplicmap.idShift), targetArray(i-1)))
    }

		print(agentmaster.putFullData(0, aplicmap.setipOffset, SimUInt32(0x0)))
		print(agentslave.putFullData(0, aplicmap.setipOffset, SimUInt32(0x0)))

		print(agentmaster.putFullData(0, aplicmap.domaincfgOffset, SimUInt32(0x80000100)))
		print(agentslave.putFullData(0, aplicmap.domaincfgOffset, SimUInt32(0x80000100)))

    // set/clripnum
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x2)))
    dut.clockDomain.waitRisingEdge(10)

    assertState(dut.aplicmaster.interrupts(1).ip, true, "master")
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x1)))
    assertState(dut.aplicmaster.interrupts(0).ip, false, "master")

    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00020002", "masterclaimi")
    assertState(dut.aplicmaster.interrupts(1).ip, false, "master")
    dut.clockDomain.waitRisingEdge(10)

    // setip and the block of setip(num) when mode is high or low
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x5)))
    assertState(dut.aplicmaster.interrupts(4).ip, true, "master")
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x6)))
    assertState(dut.aplicmaster.interrupts(5).ip, false, "master")
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x7)))
    assertState(dut.aplicmaster.interrupts(6).ip, false, "master")
    print(agentmaster.putFullData(0, aplicmap.setipOffset, SimUInt32(0b00001100)))
    assertState(dut.aplicmaster.interrupts(4).ip, false, "master")
    assertState(dut.aplicmaster.interrupts(1).ip, true, "master")
    assertState(dut.aplicmaster.interrupts(2).ip, true, "master")

    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00020002", "masterclaimi")
    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00030003", "masterclaimi")

    // input and delagation
    dut.io.sources #= 0b0111110
    dut.clockDomain.waitRisingEdge(2)
    assertState(dut.aplicslave.interrupts(0).ip, true, "slave")
    assertState(dut.aplicslave.interrupts(1).ip, false, "slave")
    assertState(dut.aplicmaster.interrupts(2).ip, true, "master")
    assertState(dut.aplicmaster.interrupts(5).ip, true, "master")
    assertState(dut.aplicmaster.interrupts(6).ip, true, "master")

    dut.io.sources #= 0b0100110
    dut.clockDomain.waitRisingEdge(2)
    assertState(dut.aplicslave.interrupts(1).ip, true, "slave")
    assertState(dut.aplicmaster.interrupts(4).ip, true, "master")

    // master
    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00030003", "masterclaimi")
    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00050005", "masterclaimi")
    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00060006", "masterclaimi")
    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00060006", "masterclaimi")

    // slave
    assertData(agentslave.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00010001", "slaveclaimi")
    dut.io.sources #= 0b0100111
    dut.clockDomain.waitRisingEdge(2)
    assertData(agentslave.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00040004", "slaveclaimi")

    dut.io.sources #= 0b1000001
    //end
    print("All sim points are success!\n")
    dut.clockDomain.waitRisingEdge(10)
  }

  class aplics() extends Component {
    val aplicslave = new TilelinkAplic(slavesourceIds, hartIds, Seq(),
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

    val aplicmaster = new TilelinkAplic(sourceIds, hartIds, Seq(aplicslave),
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

    aplicmaster.interrupts.foreach(_.ip.simPublic())
    aplicslave.interrupts.foreach(_.ip.simPublic())

    val io = new Bundle {
      val busmaster = slave(new bus.tilelink.Bus(tilelink.M2sParameters(
        sourceCount = 1,
        support = tilelink.M2sSupport(
          addressWidth = 16,
          dataWidth = 32,
          transfers = tilelink.M2sTransfers(
            get = tilelink.SizeRange(8),
            putFull = tilelink.SizeRange(8)
          )
        )
      ).toNodeParameters().toBusParameter()))
      val busslave = slave(new bus.tilelink.Bus(tilelink.M2sParameters(
        sourceCount = 1,
        support = tilelink.M2sSupport(
          addressWidth = 16,
          dataWidth = 32,
          transfers = tilelink.M2sTransfers(
            get = tilelink.SizeRange(8),
            putFull = tilelink.SizeRange(8)
          )
        )
      ).toNodeParameters().toBusParameter()))
      val sources = in Bits (7 bits)
      val targetsmaster = out Bits (2 bits)
      val targetsslave = out Bits (2 bits)
    }
    io.busmaster <> aplicmaster.io.bus
    io.busslave <> aplicslave.io.bus
    aplicmaster.io.sources := io.sources
    io.targetsmaster := aplicmaster.io.targets
    io.targetsslave := aplicslave.io.targets
    aplicslave.io.sources := aplicmaster.io.slaveSources(0)
  }

  def assertState(signal : Bool, state : Boolean, name : String) : Unit = {
    assert(signal.toBoolean == state, s"$name: missmatch (${signal} != ${state.toString()})")
  }

  def assertData(data : tilelink.sim.TransactionD, answer : String, name : String) : Unit = {
    val claimi = getdata(data.data)
    assert(claimi == answer, s"masterclaimi: missmatch (${claimi} != 0x$answer)")
  }

  def getdata(data : Array[Byte]): String = {
    val buf = new StringBuilder()
    for(i <- 0 until data.size){
      buf ++= f"${data(data.size - 1 - i)}%02x"
    }
    buf.toString()
  }
}
