package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import config.Config
import _root_.sim._
import scala.reflect.io.Streamable.Bytes

case class TilelinkAPLICFiberTest(hartIds: Seq[Int], sourceIds: Seq[Int], slavesourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val slaveInfos = Seq(APlicSlaveInfo(1, slavesourceIds))

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access at 0x00000000 of masterBus.node

    val aplicslave = TilelinkAPLICFiber()
    aplicslave.node at 0x10000000 of access

    val targetsSBundles = hartIds.map(aplicslave.addTarget(_, InterruptNode.slave()))

    val aplicmaster = TilelinkAPLICFiber()
    aplicmaster.node at 0x20000000 of access

    val sourcesMBundles = sourceIds.map(aplicmaster.addSource(_, InterruptNode.master()))
    val targetsMBundles = hartIds.map(aplicmaster.addTarget(_, InterruptNode.slave()))

    val slaveSources = slaveInfos.map(aplicmaster.addSlave(_))

    // XXX: there is only one slave
    val sourcesSBundles = slavesourceIds.zip(slaveSources(0).flags).map {
      case (id, slaveSource) => aplicslave.addSource(id, slaveSource)
    }
  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Bits(sourceIds.size bits)
    val targetsmaster = out Bits(hartIds.size bits)
    val targetsslave = out Bits(hartIds.size bits)
  }

  masterBus.bus = Some(io.bus)

  peripherals.sourcesMBundles.lazyZip(io.sources.asBools).foreach(_.flag := _)

  io.targetsmaster := peripherals.targetsMBundles.map(_.flag).asBits()
  io.targetsslave := peripherals.targetsSBundles.map(_.flag).asBits()
}

object APlicSim extends App {
  val sourcenum = 8
  val hartnum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = IndexedSeq(1, 4)
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping.aplicMap

  val compile = Config.sim.compile {
    val aplicsFiber = new TilelinkAPLICFiberTest(hartIds, sourceIds, slavesourceIds)
    aplicsFiber
  }

  compile.doSim{ dut =>
    dut.clockDomain.forkStimulus(10)

    dut.io.sources #= 0b1000001

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    val slaveoffset = 0x10000000
    val masteroffset = 0x20000000

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
    for (i <- 1 until sourcenum) {
      print(agent.putFullData(0, masteroffset + aplicmap.sourcecfgOffset + ((i - 1) * 4), prioArrayM(i-1)))
      print(agent.putFullData(0, slaveoffset + aplicmap.sourcecfgOffset + ((i - 1) * 4), prioArrayS(i-1)))
    }

    print(agent.putFullData(0, masteroffset + aplicmap.setieOffset, SimUInt32(0xff)))
    print(agent.putFullData(0, slaveoffset + aplicmap.setieOffset, SimUInt32(0xff)))

    val targetArray = Seq(SimUInt32(0x1),
                          SimUInt32(0x2),
                          SimUInt32(0x3),
                          SimUInt32(0x4),
                          SimUInt32(0x5),
                          SimUInt32(0x6),
                          SimUInt32(0x40007))
    for (i <- 1 until sourcenum) {
      print(agent.putFullData(0, masteroffset + aplicmap.targetOffset + ((i - 1) * 4), targetArray(i-1)))
      print(agent.putFullData(0, slaveoffset + aplicmap.targetOffset + ((i - 1) * 4), targetArray(i-1)))
    }

    print(agent.putFullData(0, masteroffset + aplicmap.setipOffset, SimUInt32(0x0)))
    print(agent.putFullData(0, slaveoffset + aplicmap.setipOffset, SimUInt32(0x0)))

    print(agent.putFullData(0, masteroffset + aplicmap.domaincfgOffset, SimUInt32(0x80000100)))
    print(agent.putFullData(0, slaveoffset + aplicmap.domaincfgOffset, SimUInt32(0x80000100)))

    // set/clripnum
    print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x2)))
    dut.clockDomain.waitRisingEdge(10)

    val aplicmaster = dut.peripherals.aplicmaster.core.aplic
    val aplicslave = dut.peripherals.aplicslave.core.aplic

    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000004, "master_ip")
    print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x1)))
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000004, "master_ip")

    assertData(agent.get(0, masteroffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00020002, "master_claimi")
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000000, "master_ip")
    dut.clockDomain.waitRisingEdge(10)

    // setip and the block of setip(num) when mode is high or low
    print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x5)))
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000020, "master_ip")
    print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x6)))
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000020, "master_ip")
    print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x7)))
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000020, "master_ip")
    print(agent.putFullData(0, masteroffset + aplicmap.setipOffset, SimUInt32(0b00001100)))
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x0000000c, "master_ip")

    assertData(agent.get(0, masteroffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00020002, "master_claimi")
    assertData(agent.get(0, masteroffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00030003, "master_claimi")
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000000, "master_ip")

    // input and delagation
    dut.io.sources #= 0b0111110
    dut.clockDomain.waitRisingEdge(2)
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x000000c8, "master_ip")
    assertData(agent.get(0, slaveoffset + aplicmap.setipOffset, 4), 0x00000002, "slave_ip")


    dut.io.sources #= 0b0100110
    dut.clockDomain.waitRisingEdge(2)
    assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x000000e8, "master_ip")
    assertData(agent.get(0, slaveoffset + aplicmap.setipOffset, 4), 0x00000012, "slave_ip")

    // master
    assertData(agent.get(0, masteroffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00030003, "master_claimi")
    assertData(agent.get(0, masteroffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00050005, "master_claimi")
    assertData(agent.get(0, masteroffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00060006, "master_claimi")
    assertData(agent.get(0, masteroffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00060006, "master_claimi")

    // slave
    assertData(agent.get(0, slaveoffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00010001, "slave_claimi")
    dut.io.sources #= 0b0100111
    dut.clockDomain.waitRisingEdge(2)
    assertData(agent.get(0, slaveoffset + aplicmap.idcOffset + aplicmap.claimiOffset, 4), 0x00040004, "slave_claimi")

    dut.io.sources #= 0b1000001
    //end
    print("All sim points are success!\n")
    dut.clockDomain.waitRisingEdge(10)
  }

  def assertData(data: tilelink.sim.TransactionD, answer: Int, name: String): Unit = {
    val value = data.data
    val result = value.zip(answer.toBytes).forall { case (x, y) => x == y }
    assert(result, s"$name: missmatch (${value.toList} != 0x${answer.toBytes.slice(0, 4)})")
  }
}
