package aia

import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester, SpinalSimFunSuite}
import _root_.sim._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import aia.APlicMapping.idcGroupSize

case class APlicFiberTest(hartIds: Seq[Int], sourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val M = TilelinkAPLICFiber()
    M.up at 0x10000000 of access
    crossBar << M.down

    M.domainParam = Some(new APlicDomainParam(true, true, APlicGenParam.full))

    val targetsMBundles = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      M.mapDownInterrupt(hartId, node)
      node
    })

    val sourcesMBundles = sourceIds.map(sourceId => {
      val node = InterruptNode.master()
      M.mapUpInterrupt(sourceId, node)
      node
    })
  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Bits(sourceIds.size bits)
    val targetsmaster = out Bits(hartIds.size bits)
  }

  masterBus.bus = Some(io.bus)

  peripherals.sourcesMBundles.lazyZip(io.sources.asBools).foreach(_.flag := _)

  io.targetsmaster := peripherals.targetsMBundles.map(_.flag).asBits()
}

class APlicTest extends SpinalSimFunSuite {
  onlyVerilator()

  var compile: SimCompiled[APlicFiberTest] = null

  val sourcenum = 64
  val hartnum = 8

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping

  def doCompile(): Unit ={
    compile = SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicFiberTest(hartIds, sourceIds)
    )
  }

  def assertData(data: tilelink.sim.TransactionD, answer: Int, name: String): Unit = {
    val value = data.data
    val result = value.zip(answer.toBytes).forall { case (x, y) => x == y }
    assert(result, s"$name: missmatch (${value.toList} != 0x${answer.toBytes.slice(0, 4)})")
  }

  def assertBit(data: tilelink.sim.TransactionD, id: Int, answer: Int, name: String = ""): Unit = {
    val value = data.data
    val byteIndex = id / 8
    val bitIndex = id % 8

    val bit = (value(byteIndex) >> bitIndex) & 1
    val result = bit == answer

    assert(result, s"$name: missmatch (${value.toList} != 0x${answer.toBytes.slice(0, 4)})")
  }

  test("APlic Direct") {
    doCompile()

    compile.doSim{ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)
    
      val baseaddr = 0x10000000

      val configs = ArrayBuffer[gateway]()
      for (i <- 0 until sourcenum) {
        val config = new gateway(i)
        config.mode = sourceMode.random()
        config.hartId = i % 8
        config.iprio = 1
        configs += config
      }

      // TODO: move into def
      agent.putFullData(0, baseaddr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))

      for (i <- 0 until sourcenum) {
        setMode(agent, baseaddr, i * 4, configs(i))
      }

      for (i <- 0 until hartnum) {
        agent.putFullData(0, baseaddr + aplicmap.idcOffset + i * idcGroupSize, SimUInt32(0x1))
      }

      agent.putFullData(0, baseaddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)
      dut.io.sources #= 0xffffff



      dut.clockDomain.waitRisingEdge(20)
    }
  }

  case class gateway(id: Int) {
    var mode = sourceMode.INACTIVE
    var hartId = 0
    var iprio = 0

    def target = SimUInt32((iprio | (hartId << 18)) & 0xFFFFFFFF)

    // maybe replace 'offset' by 'id'
    def assertIEP(answer: Int, agent: tilelink.sim.MasterAgent, base: Int, offset: Int): Unit = {
      mode match {
        case sourceMode.INACTIVE =>
          assertBit(agent.get(0, base + offset, 4), id, 0)

        // manual judge
        case sourceMode.DETACHED =>
          assertBit(agent.get(0, base + offset, 4), id, 0)
          
        // auto judge?
        case sourceMode.EDGE1 | sourceMode.EDGE0 => 
          assertBit(agent.get(0, base + offset, 4), id, answer)
        case sourceMode.LEVEL1 | sourceMode.LEVEL0 =>
          assertBit(agent.get(0, base + offset, 4), id, answer)
      }
      assertBit(agent.get(0, base + offset, 4), id, answer)
    }
  }

  object sourceMode extends Enumeration {
    type mode = Value
    val INACTIVE, DETACHED, EDGE1, EDGE0, LEVEL1, LEVEL0 = Value

    def random(): Value = {
      val values = sourceMode.values.toSeq
      values(Random.nextInt(values.length))
    }
  }

  def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, gateway: gateway): Unit = {
    gateway.mode match {
      case sourceMode.INACTIVE => agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x0))
      case sourceMode.DETACHED => agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x1))
      case sourceMode.EDGE1 => agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x4))
      case sourceMode.EDGE0 => agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x5))
      case sourceMode.LEVEL1 => agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x6))
      case sourceMode.LEVEL0 => agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x7))
    }

    val sourceId = offset / 4
    if (gateway.mode == sourceMode.INACTIVE) {
      agent.putFullData(0, base + aplicmap.clrienumOffset, SimUInt32(sourceId))
    } else {
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(sourceId))
    }

    agent.putFullData(0, base + aplicmap.targetOffset + offset, gateway.target)
  }
}
