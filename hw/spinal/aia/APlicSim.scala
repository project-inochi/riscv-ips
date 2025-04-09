package aia

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import config.Config
import _root_.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.core.fiber.Fiber

object APlicSim extends App {
  val sourcenum = 1023
  val hartnum = 1023

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = IndexedSeq(1, 4)
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping.aplicMap

  val compile = Config.sim.compile{
    val aplics = new aplics(hartIds, sourceIds, slavesourceIds)
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

    val aplicmaster = dut.aplicmaster.aplic
    val aplicslave = dut.aplicslave.aplic

    assertState(aplicmaster.interrupts(1).ip, true, "master")
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x1)))
    assertState(aplicmaster.interrupts(0).ip, false, "master")

    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00020002", "masterclaimi")
    assertState(aplicmaster.interrupts(1).ip, false, "master")
    dut.clockDomain.waitRisingEdge(10)

    // setip and the block of setip(num) when mode is high or low
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x5)))
    assertState(aplicmaster.interrupts(4).ip, true, "master")
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x6)))
    assertState(aplicmaster.interrupts(5).ip, false, "master")
    print(agentmaster.putFullData(0, aplicmap.setipnumOffset, SimUInt32(0x7)))
    assertState(aplicmaster.interrupts(6).ip, false, "master")
    print(agentmaster.putFullData(0, aplicmap.setipOffset, SimUInt32(0b00001100)))
    assertState(aplicmaster.interrupts(4).ip, false, "master")
    assertState(aplicmaster.interrupts(1).ip, true, "master")
    assertState(aplicmaster.interrupts(2).ip, true, "master")

    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00020002", "masterclaimi")
    assertData(agentmaster.get(0, aplicmap.idcOffset + aplicmap.claimiOffset, 4), "00030003", "masterclaimi")

    // input and delagation
    dut.io.sources #= 0b0111110
    dut.clockDomain.waitRisingEdge(2)
    assertState(aplicslave.interrupts(0).ip, true, "slave")
    assertState(aplicslave.interrupts(1).ip, false, "slave")
    assertState(aplicmaster.interrupts(2).ip, true, "master")
    assertState(aplicmaster.interrupts(5).ip, true, "master")
    assertState(aplicmaster.interrupts(6).ip, true, "master")

    dut.io.sources #= 0b0100110
    dut.clockDomain.waitRisingEdge(2)
    assertState(aplicslave.interrupts(1).ip, true, "slave")
    assertState(aplicmaster.interrupts(4).ip, true, "master")

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

case class aplics(hartIds : Seq[Int], sourceIds : Seq[Int], slavesourceIds: Seq[Int]) extends Component {
  val busParam = tilelink.M2sParameters(
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

  val aplicslave = new TilelinkAplic(slavesourceIds, hartIds, Seq(), busParam)
  val aplicmaster = new TilelinkAplic(sourceIds, hartIds, Seq(), busParam)

  aplicmaster.io.simPublic()
  aplicslave.io.simPublic()
  aplicmaster.aplic.interrupts.foreach(_.ip.simPublic())
  aplicslave.aplic.interrupts.foreach(_.ip.simPublic())

  val io = new Bundle {
    val busmaster = slave(new bus.tilelink.Bus(busParam))
    val busslave = slave(new bus.tilelink.Bus(busParam))
    val sources = in Bits (sourceIds.size bits)
    val targetsmaster = out Bits (hartIds.size bits)
    val targetsslave = out Bits (hartIds.size bits)
  }
  io.busmaster <> aplicmaster.io.bus
  io.busslave <> aplicslave.io.bus

  aplicmaster.io.sources := io.sources
  io.targetsmaster := aplicmaster.io.targets
  io.targetsslave := aplicslave.io.targets
  aplicslave.io.sources := aplicmaster.io.slaveSources(0)
}

case class TilelinkAPLICFiberTest(hartIds : Seq[Int], sourceIds : Seq[Int], slavesourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val sourcesMBundle = for (sourceId <- sourceIds) yield new APlicBundle(sourceId)
  val targetsMBundle = for (hartId <- hartIds) yield new APlicBundle(hartId)
  val slaveinfo = new APlicSlaveInfo(1, slavesourceIds)
  val slavesourceMBundle = for (slavesourceId <- slavesourceIds) yield new APlicBundle(slavesourceId)

  val sourcesSBundle = for (sourceId <- slavesourceIds) yield new APlicBundle(sourceId)
  val targetsSBundle = for (hartId <- hartIds) yield new APlicBundle(hartId)

  val peripherals = new Area{
    val access = tilelink.fabric.Node()
    access at 0x00000000 of masterBus.node

    val aplicslave = TilelinkAPLICFiber()
    aplicslave.node at 0x00000000 of access

    for (source <- sourcesSBundle){
      aplicslave.addsource(source)
    }
    for (target <- targetsSBundle){
      aplicslave.addtarget(target)
    }

    val aplicmaster = TilelinkAPLICFiber()
    aplicmaster.node at 0x10000000 of access

    for (source <- sourcesMBundle){
      aplicmaster.addsource(source)
    }
    for (target <- targetsMBundle){
      aplicmaster.addtarget(target)
    }
    aplicmaster.addslave(slaveinfo)
    for (slaveBundle <- slavesourceMBundle){
      aplicmaster.addslavesource(slaveBundle)
    }
  }

  val io = new Bundle{
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Vec(Bits(sourceIds.size bits))
    val targetsmaster = out Vec(Bits(hartIds.size bits))
    val targetsslave = out Vec(Bits(hartIds.size bits))
  }

  masterBus.bus = Some(io.bus)

  // Vec(sourcesBundle.map(_.flag).asBits) := io.sources Why?
  (sourcesMBundle.map(_.flag), io.sources(0).asBools).zipped.foreach(_ := _)
  io.targetsmaster := Vec(targetsMBundle.map(_.flag).asBits)
  io.targetsslave := Vec(targetsSBundle.map(_.flag).asBits)

  (sourcesSBundle.map(_.flag), slavesourceMBundle.map(_.flag)).zipped.foreach(_ := _)
}

object APlicNodeSim extends App {
  val sourcenum = 8
  val hartnum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = IndexedSeq(1, 4)
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping.aplicMap

  val compile = Config.sim.compile{
    val aplicsFiber = new TilelinkAPLICFiberTest(hartIds, sourceIds, slavesourceIds)
    aplicsFiber
  }

  compile.doSim{ dut =>
		dut.clockDomain.forkStimulus(10)

    dut.io.sources(0) #= 0b0000000

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    val masteroffset = 0x10000000
    val slaveoffset = 0x00000000

    print(agent.putFullData(0, masteroffset + aplicmap.sourcecfgOffset + 4, SimUInt32(0x400)))
    print(agent.putFullData(0, masteroffset + aplicmap.sourcecfgOffset + 8, SimUInt32(0x1)))
    print(agent.putFullData(0, masteroffset + aplicmap.targetOffset + 8, SimUInt32(0x2)))
    print(agent.putFullData(0, masteroffset + aplicmap.setieOffset, SimUInt32(0xff)))
    print(agent.putFullData(0, masteroffset + aplicmap.setipOffset, SimUInt32(0x0)))

    print(agent.putFullData(0, slaveoffset + aplicmap.sourcecfgOffset + 4, SimUInt32(0x6)))
    print(agent.putFullData(0, slaveoffset + aplicmap.targetOffset + 4, SimUInt32(0x2)))
    print(agent.putFullData(0, slaveoffset + aplicmap.setieOffset, SimUInt32(0xff)))
    print(agent.putFullData(0, slaveoffset + aplicmap.setipOffset, SimUInt32(0x0)))

    print(agent.putFullData(0, slaveoffset + aplicmap.domaincfgOffset, SimUInt32(0x80000100)))
    print(agent.putFullData(0, masteroffset + aplicmap.domaincfgOffset, SimUInt32(0x80000100)))

    print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x2)))

    dut.io.sources(0) #= 0b0000001

    dut.clockDomain.waitRisingEdge(10)

	}
}
