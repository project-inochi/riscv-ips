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

case class APlicFiberTest(hartIds: Seq[Int], sourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val blocks = for (hartId <- hartIds) yield new SxAIABlock(sourceIds, hartId, 0)

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val M = TilelinkAPLICFiber()
    M.up at 0x10000000 of access
    crossBar << M.down

    val dispatcher = TilelinkIMSICFiber()
    dispatcher.node at 0x30000000 of access

    for (block <- blocks) {
      val trigger = dispatcher.addIMSICinfo(block.asTilelinkIMSICIInfo())
      val connector = SxAIABlockTrigger(block, trigger)
    }

    M.domainParam = Some(APlicDomainParam.root(APlicGenParam.full))

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
    val ie = in Vec(blocks.map(block => Bits(block.interrupts.size bits)))
    val ip = out Vec(blocks.map(block => Bits(block.interrupts.size bits)))
  }

  masterBus.bus = Some(io.bus)

  peripherals.sourcesMBundles.lazyZip(io.sources.asBools).foreach(_.flag := _)

  io.targetsmaster := peripherals.targetsMBundles.map(_.flag).asBits()

  Vec(blocks.map(block => block.interrupts.map(_.ie).asBits())) := io.ie
  io.ip := Vec(blocks.map(block => block.interrupts.map(_.ip).asBits()))
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
    val idx = id % 32
    val byteIndex = idx / 8
    val bitIndex = idx % 8

    val bit = (value(byteIndex) >> bitIndex) & 1
    val result = bit == answer

    assert(result, s"$name: missmatch value = (${value.toList}")
  }

  def assertIP(dut: APlicFiberTest, sourceIO: BigInt, configs: ArrayBuffer[gateway]) = {
    dut.io.sources #= sourceIO
    for ((config, i) <- configs.zipWithIndex) {
      if (config.mode != sourceMode.DETACHED) {
        config.assertIP(sourceIO.testBit(i).toInt)
      }
    }
  }

  /* 
   * TODO:
   * direct mode ifore test
   * 
   */

  test("APlic Direct") {
    doCompile()

    compile.doSim("APlic Direct"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)
    
      val baseaddr = 0x10000000

      // default data test BEGAIN
      // domaincfg
      assertData(agent.get(0, baseaddr + aplicmap.domaincfgOffset, 4), 0x80000000, "def_domaincfg")

      // mmsiaddrcfg/h
      assertData(agent.get(0, baseaddr + aplicmap.mmsiaddrcfgOffset, 4), 0x0, "def_mmsiaddrcfg")
      assertData(agent.get(0, baseaddr + aplicmap.mmsiaddrcfghOffset, 4), 0x0, "def_mmsiaddrcfgh")
      
      // smsiaddrcfg/h
      assertData(agent.get(0, baseaddr + aplicmap.smsiaddrcfgOffset, 4), 0x0, "def_smsiaddrcfg")
      assertData(agent.get(0, baseaddr + aplicmap.smsiaddrcfghOffset, 4), 0x0, "def_smsiaddrcfgh")

      // setiep
      for (i <- 0 until sourcenum/32) {
        assertData(agent.get(0, baseaddr + aplicmap.setipOffset + i * 4, 4), 0x0, "def_setipcfg")
        assertData(agent.get(0, baseaddr + aplicmap.setieOffset + i * 4, 4), 0x0, "def_setiecfg")
      }

      assertData(agent.get(0, baseaddr + aplicmap.setipnumOffset, 4), 0x0, "def_setipnumcfg")
      assertData(agent.get(0, baseaddr + aplicmap.setienumOffset, 4), 0x0, "def_setienumcfg")
      // should return rectified value, re-test below. ch:4.5.7
      assertData(agent.get(0, baseaddr + aplicmap.in_clripOffset, 4), 0x0, "def_in_clripcfg1")
      assertData(agent.get(0, baseaddr + aplicmap.clripnumOffset, 4), 0x0, "def_clripnumcfg")
      assertData(agent.get(0, baseaddr + aplicmap.clrieOffset, 4), 0x0, "def_clrieOffsetcfg")
      assertData(agent.get(0, baseaddr + aplicmap.clrienumOffset, 4), 0x0, "def_clrienumOffsetcfg")
      // setipnum_le/be not implementation
      assertData(agent.get(0, baseaddr + aplicmap.genmsiOffset, 4), 0x0, "def_genmsicfg")

      for (i <- 0 until sourcenum - 1) {
        assertData(agent.get(0, baseaddr + aplicmap.sourcecfgOffset + i * 4, 4), 0x0, "def_sourcecfg")
        assertData(agent.get(0, baseaddr + aplicmap.targetOffset + i * 4, 4), 0x1, "def_targetcfg")
      }

      for (i <- 0 until hartnum) {
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, 4), 0x0, "def_ideliverycfg")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.iforceOffset + i * aplicmap.idcGroupSize, 4), 0x0, "def_iforcecfg")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.ithresholdOffset + i * aplicmap.idcGroupSize, 4), 0x0, "def_ithresholdcfg")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.topiOffset + i * aplicmap.idcGroupSize, 4), 0x0, "def_topicfg")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.claimiOffset + i * aplicmap.idcGroupSize, 4), 0x0, "def_claimicfg")
      }
      // default data test END

      agent.putFullData(0, baseaddr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))

      val configs = ArrayBuffer[gateway]()
      for (i <- 1 until sourcenum) {
        val mode = sourceMode.random()
        val config = createGateway(mode, i, agent, baseaddr)
        config.hartId = Random.nextInt(hartnum)
        config.iprio = 1
        config.setMode(agent, baseaddr, (i-1)*4)
        configs += config
      }

      for (i <- 0 until hartnum) {
        agent.putFullData(0, baseaddr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))
      }

      agent.putFullData(0, baseaddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      // setie 4.5.12 - 4.5.14
      for ((config, i) <- configs.zipWithIndex) {
        if (config.mode != sourceMode.INACTIVE) {
          config.assertIE()
        } else {
          config.assertIE()
        }
      }
      // source => setip 4.5.5
      var sourceIO = BigInt("0", 16)
      assertIP(dut, sourceIO, configs)

      sourceIO = BigInt("7fffffffffffffff", 16)
      assertIP(dut, sourceIO, configs)

      sourceIO = BigInt("0", 16)
      assertIP(dut, sourceIO, configs)

      for ((config, i) <- configs.zipWithIndex) {
        if (config.mode == sourceMode.LEVEL0) {
          sourceIO |= (BigInt(1) << i)
        }
      }

      // 4.5.7
      assertData(agent.get(0, baseaddr + aplicmap.in_clripOffset, 4), ((sourceIO & ((BigInt(1) << 31) - 1))<<1).toInt, "def_in_clripcfg2")
      assertData(agent.get(0, baseaddr + aplicmap.in_clripOffset + 4, 4), ((sourceIO >> 31) & ((BigInt(1) << 32) - 1)).toInt, "def_in_clripcfg3")

      dut.io.sources #= sourceIO
      dut.clockDomain.waitRisingEdge(2)

      // claimi 4.8.1.5
      for ((config, i) <- configs.zipWithIndex) {
        if (Set(sourceMode.EDGE0, sourceMode.EDGE1).contains(config.mode)) {
          assertData(agent.get(0, baseaddr + aplicmap.idcOffset + config.hartId * aplicmap.idcGroupSize + aplicmap.claimiOffset, 4),
          (config.iprio | (config.idx << 16)) & 0xFFFFFFFF, "claimi_io")
          config.ip = 0
        }
      }

      // setipnum 4.6.5
      for ((config, i) <- configs.zipWithIndex) {
        if (Set(sourceMode.EDGE0, sourceMode.EDGE1, sourceMode.DETACHED).contains(config.mode)) {
          config.ip = 1
        }
        agent.putFullData(0, baseaddr + aplicmap.setipnumOffset, SimUInt32(i+1))
      }
      for ((config, i) <- configs.zipWithIndex) {
        if (Set(sourceMode.EDGE0, sourceMode.EDGE1, sourceMode.DETACHED).contains(config.mode)) {
          assertData(agent.get(0, baseaddr + aplicmap.idcOffset + config.hartId * aplicmap.idcGroupSize + aplicmap.claimiOffset, 4),
          (config.iprio | (config.idx << 16)) & 0xFFFFFFFF, "claimi_setipnum")
          config.ip = 0
        }
      }

      // setip 4.5.5
      agent.putFullData(0, baseaddr + aplicmap.setipOffset, SimUInt32(0xffffffff))
      agent.putFullData(0, baseaddr + aplicmap.setipOffset + 4, SimUInt32(0xffffffff))
      for ((config, i) <- configs.zipWithIndex) {
        if (Set(sourceMode.EDGE0, sourceMode.EDGE1, sourceMode.DETACHED).contains(config.mode)) {
          assertData(agent.get(0, baseaddr + aplicmap.idcOffset + config.hartId * aplicmap.idcGroupSize + aplicmap.claimiOffset, 4),
          (config.iprio | (config.idx << 16)) & 0xFFFFFFFF, "claimi_setip")
          config.ip = 0
        }
      }

      dut.clockDomain.waitRisingEdge(20)
    }
  }

  test("APlic MSI") {
    doCompile()

    compile.doSim("APlic MSI"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0
      dut.io.ie.map(_ #= BigInt("7fffffffffffffff", 16))

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)
    
      val aplicAddr = 0x10000000
      val imsicAddr = 0x30000000

      agent.putFullData(0, aplicAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000004))

      val configs = ArrayBuffer[gateway]()
      for (i <- 1 until sourcenum) {
        val mode = sourceMode.EDGE1
        val config = createGateway(mode, i, agent, aplicAddr)
        config.hartId = if (i < 32) 0 else 1
        config.deliveryMode = true
        config.setMode(agent, aplicAddr, (i-1)*4)
        configs += config
      }

      agent.putFullData(0, aplicAddr + aplicmap.mmsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
      agent.putFullData(0, aplicAddr + aplicmap.mmsiaddrcfghOffset, SimUInt32(0x3000))
      agent.putFullData(0, aplicAddr + aplicmap.smsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
      agent.putFullData(0, aplicAddr + aplicmap.smsiaddrcfghOffset, SimUInt32(0x0))

      agent.putFullData(0, aplicAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000104))

      dut.clockDomain.waitRisingEdge(10)

      var sourceIO = BigInt("0", 16)
      for ((config, i) <- configs.zipWithIndex) {
        dut.io.sources #= sourceIO | (BigInt(1) << i)
        dut.clockDomain.waitRisingEdge(2)
        dut.io.sources #= sourceIO
      }

      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40001))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40002))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40003))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40004))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40005))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40006))
      // dut.io.sources #= BigInt("7fffffffffffffff", 16)

      // // dut.clockDomain.waitRisingEdge(80)
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40007))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40008))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x40009))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x4000a))
      // agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(0x4000b))

      dut.clockDomain.waitRisingEdge(100)
    }
  }

  abstract class gateway(id: Int) {
    val idx = id
    var mode = sourceMode.INACTIVE
    var ie = 0
    var ip = 0
    var deliveryMode = false
    var hartId = 0
    var iprio = 0
    var guestIndex = 0
    var eiid = id

    def target = if (deliveryMode) SimUInt32((eiid | (guestIndex << 12) | (hartId << 18)) & 0xFFFFFFFF)
                   else SimUInt32((iprio | (hartId << 18)) & 0xFFFFFFFF)

    def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int): Unit
    def assertIE(): Unit
    def assertIP(io: Int): Unit
  }

  case class inactive(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.INACTIVE

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int) = {
      ie = 0
      agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x0))
      agent.putFullData(0, base + aplicmap.clrienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie)
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip)
    }
  }

  case class detached(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.DETACHED

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int) = {
      ie = 1
      agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x1))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie)
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip)
    }
  }

  case class edge1(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.EDGE1
    var reg = 0

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int) = {
      ie = 1
      agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x4))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
        val offset = id / 32 * 4
        assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie)
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      if (reg == 0 && io == 1) {
        ip = 1
      }
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip)
      reg = io
    }
  }

  case class edge0(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.EDGE0
    var reg = 0

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int) = {
      ie = 1
      agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x5))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie)
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      if (reg == 1 && io == 0) {
        ip = 1
      }
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip)
      reg = io
    }
  }

  case class level1(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.LEVEL1

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int) = {
      ie = 1
      agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x6))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie)
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, io)
    }
  }

  case class level0(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.LEVEL0

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int) = {
      ie = 1
      agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x7))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie)
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      if (io == 0) {
        assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, 1)
      } else {
        assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, 0)
      }
    }
  }

  def createGateway(mode: sourceMode.Value, id: Int, agent: tilelink.sim.MasterAgent, baseaddr: Int): gateway = {
    mode match {
      case sourceMode.INACTIVE => inactive(id, agent, baseaddr)
      case sourceMode.DETACHED => detached(id, agent, baseaddr)
      case sourceMode.EDGE1    => edge1(id, agent, baseaddr)
      case sourceMode.EDGE0    => edge0(id, agent, baseaddr)
      case sourceMode.LEVEL1   => level1(id, agent, baseaddr)
      case sourceMode.LEVEL0   => level0(id, agent, baseaddr)
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
}
