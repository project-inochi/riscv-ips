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

case class APlicUnitFiberTest(hartIds: Seq[Int], sourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val blocks = for (hartId <- hartIds) yield new SxAIABlock(sourceIds, hartId, 0)

  val APlicGenMode = APlicGenParam.test

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

    M.domainParam = Some(APlicDomainParam.root(APlicGenMode))

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

case class APlicMSFiberTest(hartIds: Seq[Int], sourceIds: Seq[Int], slavesourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val slaveInfos = Seq(APlicSlaveInfo(1, slavesourceIds), APlicSlaveInfo(2, slavesourceIds))

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val S1 = TilelinkAPLICFiber()
    S1.up at 0x10000000 of access
    crossBar << S1.down

    val S2 = TilelinkAPLICFiber()
    S2.up at 0x20000000 of access
    crossBar << S2.down

    val M = TilelinkAPLICFiber()
    M.up at 0x30000000 of access
    crossBar << M.down

    M.domainParam = Some(APlicDomainParam.root(APlicGenParam.full))
    S1.domainParam = Some(APlicDomainParam.S(APlicGenParam.full))
    S2.domainParam = Some(APlicDomainParam.S(APlicGenParam.full))

    val targets1SBundles = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      S1.mapDownInterrupt(hartId, node)
      node
    })

    val targets2SBundles = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      S2.mapDownInterrupt(hartId, node)
      node
    })

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

    val slaveSources = slaveInfos.map(M.createInterruptDelegation(_))

    // XXX: there is only one slave
    val sourcesS1Bundles = slavesourceIds.zip(slaveSources(0).flags).map {
      case (id, slaveSource) => S1.mapUpInterrupt(id, slaveSource)
    }
    val sourcesS2Bundles = slavesourceIds.zip(slaveSources(1).flags).map {
      case (id, slaveSource) => S2.mapUpInterrupt(id, slaveSource)
    }

    S1.mmsiaddrcfg := M.mmsiaddrcfg
    S1.smsiaddrcfg := M.smsiaddrcfg
    S2.mmsiaddrcfg := M.mmsiaddrcfg
    S2.smsiaddrcfg := M.smsiaddrcfg
  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Bits(sourceIds.size bits)
    val targetsmaster = out Bits(hartIds.size bits)
    val targets1slave = out Bits(hartIds.size bits)
    val targets2slave = out Bits(hartIds.size bits)
  }

  masterBus.bus = Some(io.bus)

  peripherals.sourcesMBundles.lazyZip(io.sources.asBools).foreach(_.flag := _)

  io.targetsmaster := peripherals.targetsMBundles.map(_.flag).asBits()
  io.targets1slave := peripherals.targets1SBundles.map(_.flag).asBits()
  io.targets2slave := peripherals.targets2SBundles.map(_.flag).asBits()
}

case class APlicSystemFiberTest(hartIds: Seq[Int], sourceIds: Seq[Int], slave1sourceIds: Seq[Int], slave2sourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val blocks = for (hartId <- hartIds.drop(1)) yield new SxAIABlock(sourceIds, hartId, 0)

  val slave1Infos = Seq(APlicSlaveInfo(1, slave1sourceIds))
  val slave2Infos = Seq(APlicSlaveInfo(1, slave2sourceIds))

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val S2 = TilelinkAPLICFiber()
    S2.up at 0x10000000 of access
    crossBar << S2.down

    val S1 = TilelinkAPLICFiber()
    S1.up at 0x20000000 of access
    crossBar << S1.down

    val M = TilelinkAPLICFiber()
    M.up at 0x30000000 of access
    crossBar << M.down

    val dispatcher = TilelinkIMSICFiber()
    dispatcher.node at 0x40000000 of access

    M.domainParam = Some(APlicDomainParam.root(APlicGenParam.full))
    S1.domainParam = Some(APlicDomainParam.M(APlicGenParam.full))
    S2.domainParam = Some(APlicDomainParam.S(APlicGenParam.full))

    for (block <- blocks) {
      val trigger = dispatcher.addIMSICinfo(block.asTilelinkIMSICIInfo())
      val connector = SxAIABlockTrigger(block, trigger)
    }

    val targetsS2Bundles = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      S2.mapDownInterrupt(hartId, node)
      node
    })

    val targetsS1Bundles = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      S1.mapDownInterrupt(hartId, node)
      node
    })

    val targetsMBundles = InterruptNode.slave()
    M.mapDownInterrupt(0, targetsMBundles)

    val sourcesMBundles = sourceIds.map(sourceId => {
      val node = InterruptNode.master()
      M.mapUpInterrupt(sourceId, node)
      node
    })

    val slaveMSources = slave1Infos.map(M.createInterruptDelegation(_))
    val slaveS1Sources = slave2Infos.map(S1.createInterruptDelegation(_))

    val sourcesS2Bundles = slave2sourceIds.zip(slaveS1Sources(0).flags).map {
      case (id, slaveSource) => S2.mapUpInterrupt(id, slaveSource)
    }

    val sourcesS1Bundles = slave1sourceIds.zip(slaveMSources(0).flags).map {
      case (id, slaveSource) => S1.mapUpInterrupt(id, slaveSource)
    }

    S1.mmsiaddrcfg := M.mmsiaddrcfg
    S1.smsiaddrcfg := M.smsiaddrcfg
    S2.mmsiaddrcfg := M.mmsiaddrcfg
    S2.smsiaddrcfg := M.smsiaddrcfg
  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Bits(sourceIds.size bits)
    val ie = in Vec(blocks.map(block => Bits(block.interrupts.size bits)))
    val ip = out Vec(blocks.map(block => Bits(block.interrupts.size bits)))
    val targetsmaster = out Bits(1 bits)
    val targets1slave = out Bits(hartIds.size bits)
    val targets2slave = out Bits(hartIds.size bits)
  }

  masterBus.bus = Some(io.bus)

  peripherals.sourcesMBundles.lazyZip(io.sources.asBools).foreach(_.flag := _)

  io.targetsmaster := peripherals.targetsMBundles.flag.asBits
  io.targets1slave := peripherals.targetsS1Bundles.map(_.flag).asBits()
  io.targets2slave := peripherals.targetsS2Bundles.map(_.flag).asBits()

  Vec(blocks.map(block => block.interrupts.map(_.ie).asBits())) := io.ie
  io.ip := Vec(blocks.map(block => block.interrupts.map(_.ip).asBits()))
}

/*
 * TODO:
 * cs2?
 * remove masterBus
 * guest id
 * msi lock
 *
 */

class APlicUnitTest extends APlicTest {
  var compile: SimCompiled[APlicUnitFiberTest] = null

  val sourcenum = 64
  val hartnum = 8

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i

  def doCompile(): Unit ={
    compile = SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicUnitFiberTest(hartIds, sourceIds)
    )
  }

  test("Direct") {
    doCompile()

    compile.doSim("Direct"){ dut =>
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
        assertData(agent.get(0, baseaddr + aplicmap.setipOffset + i * 4, 4), 0x0, s"def_setipcfg_$i")
        assertData(agent.get(0, baseaddr + aplicmap.setieOffset + i * 4, 4), 0x0, s"def_setiecfg_$i")
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
        assertData(agent.get(0, baseaddr + aplicmap.sourcecfgOffset + i * 4, 4), 0x0, s"def_sourcecfg_$i")
        assertData(agent.get(0, baseaddr + aplicmap.targetOffset + i * 4, 4), 0x1, s"def_targetcfg_$i")
      }

      for (i <- 0 until hartnum) {
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"def_ideliverycfg_$i")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.iforceOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"def_iforcecfg_$i")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.ithresholdOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"def_ithresholdcfg_$i")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.topiOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"def_topicfg_$i")
        assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.claimiOffset + i * aplicmap.idcGroupSize, 4), 0x0, s"def_claimicfg_$i")
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
      dut.io.sources #= sourceIO
      assertIP(sourceIO, configs)

      sourceIO = BigInt("7fffffffffffffff", 16)
      dut.io.sources #= sourceIO
      assertIP(sourceIO, configs)

      sourceIO = BigInt("0", 16)
      dut.io.sources #= sourceIO
      assertIP(sourceIO, configs)

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
          (config.iprio | (config.idx << 16)) & 0xFFFFFFFF, s"claimi_io_$i")
          config.ip = 0
        }
      }

      // iforce
      if (dut.APlicGenMode.withIForce) {
        for (i <- 0 until hartnum) {
          agent.putFullData(0, baseaddr + aplicmap.idcOffset + aplicmap.iforceOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))

          val ipIO = dut.io.targetsmaster.toBigInt
          assertIO(ipIO, i, 1, s"iforce output_$i")
          assertData(agent.get(0, baseaddr + aplicmap.idcOffset + aplicmap.claimiOffset + i * aplicmap.idcGroupSize, 4),
            0, s"iforce_$i")
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
          (config.iprio | (config.idx << 16)) & 0xFFFFFFFF, s"claimi_setipnum_$i")
          config.ip = 0
        }
      }

      // setip 4.5.5
      agent.putFullData(0, baseaddr + aplicmap.setipOffset, SimUInt32(0xffffffff))
      agent.putFullData(0, baseaddr + aplicmap.setipOffset + 4, SimUInt32(0xffffffff))
      for ((config, i) <- configs.zipWithIndex) {
        if (Set(sourceMode.EDGE0, sourceMode.EDGE1, sourceMode.DETACHED).contains(config.mode)) {
          assertData(agent.get(0, baseaddr + aplicmap.idcOffset + config.hartId * aplicmap.idcGroupSize + aplicmap.claimiOffset, 4),
          (config.iprio | (config.idx << 16)) & 0xFFFFFFFF, s"claimi_setip_$i")
          config.ip = 0
        }
      }

      dut.clockDomain.waitRisingEdge(20)
    }
  }

  test("MSI") {
    doCompile()

    compile.doSim("MSI"){ dut =>
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
      var ipIO = BigInt("0", 16)
      var randomHartid = 0

      for ((config, i) <- configs.zipWithIndex) {
        // 4.5.16
        dut.io.sources #= sourceIO | (BigInt(1) << i)
        dut.clockDomain.waitRisingEdge(2)
        dut.io.sources #= sourceIO
        dut.clockDomain.waitRisingEdge(2)
        ipIO = dut.io.ip(config.hartId).toBigInt
        assertIO(ipIO, i, 1, s"assert gateway ip output_$i")

        // wait busy bit 4.5.15
        Iterator
          .continually((agent.get(0, aplicAddr + aplicmap.genmsiOffset, 4).data(1) & 0x10) != 0)
          .takeWhile(identity)
          .foreach{_ => }

        randomHartid = Random.between(1, hartnum)
        agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(randomHartid << 18 | i+1))
        dut.clockDomain.waitRisingEdge(2)

        ipIO = dut.io.ip(randomHartid).toBigInt
        assertIO(ipIO, i, 1, s"assert genmsi ip output_$i")
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }
}

class APlicMSTest extends APlicTest {
  var compile: SimCompiled[APlicMSFiberTest] = null

  val sourcenum = 64
  val hartnum = 8

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = (1 to 63).toIndexedSeq
  val hartIds = for (i <- 0 until hartnum) yield i

  def doCompile(): Unit ={
    compile = SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicMSFiberTest(hartIds, sourceIds, slavesourceIds)
    )
  }

  test("MS Direct") {
    doCompile()

    compile.doSim("MS Direct"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      val masterAddr = 0x30000000
      val slave1Addr = 0x10000000
      val slave2Addr = 0x20000000

      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))
      agent.putFullData(0, slave1Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))
      agent.putFullData(0, slave2Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))

      val candidates = (1 to 63).toSet
      val shuffledCandidates = Random.shuffle(candidates.toList).take(32)

      val randomIds_slave1 = shuffledCandidates.take(16)
      val randomIds_slave2 = shuffledCandidates.drop(16)

      val masterconfigs = ArrayBuffer[gateway]()
      for (i <- 1 until sourcenum) {
        val isDelegaton = randomIds_slave1.contains(i) || randomIds_slave2.contains(i)
        val mode = if (isDelegaton) sourceMode.INACTIVE else sourceMode.random()
        val config = createGateway(mode, i, agent, masterAddr)
        config.hartId = Random.nextInt(hartnum)
        config.iprio = 1
        config.setMode(agent, masterAddr, (i-1)*4, (if (isDelegaton && randomIds_slave1.contains(i)) 1 else if (isDelegaton) 2 else 0))
        masterconfigs += config
      }

      val slave1configs = ArrayBuffer[gateway]()
      val slave2configs = ArrayBuffer[gateway]()
      for (i <- candidates) {
        var isDelegaton = randomIds_slave1.contains(i)
        var mode = if (isDelegaton) sourceMode.random() else sourceMode.INACTIVE
        val slave1config = createGateway(mode, i, agent, slave1Addr)
        slave1config.hartId = Random.nextInt(hartnum)
        slave1config.iprio = 1
        slave1config.setMode(agent, slave1Addr, (i-1)*4, 0)
        slave1configs += slave1config

        isDelegaton = randomIds_slave2.contains(i)
        mode = if (isDelegaton) sourceMode.random() else sourceMode.INACTIVE
        val slave2config = createGateway(mode, i, agent, slave2Addr)
        slave2config.hartId = Random.nextInt(hartnum)
        slave2config.iprio = 1
        slave2config.setMode(agent, slave2Addr, (i-1)*4, 0)
        slave2configs += slave2config
      }

      for (i <- 0 until hartnum) {
        agent.putFullData(0, masterAddr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))
        agent.putFullData(0, slave1Addr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))
        agent.putFullData(0, slave2Addr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))
      }

      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
      agent.putFullData(0, slave1Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
      agent.putFullData(0, slave2Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))

      dut.clockDomain.waitRisingEdge(10)

      var sourceIO = BigInt("0", 16)
      dut.io.sources #= sourceIO
      assertIP(sourceIO, masterconfigs)
      assertIP(sourceIO, slave1configs)
      assertIP(sourceIO, slave2configs)
      dut.clockDomain.waitRisingEdge(10)

      sourceIO = BigInt("7fffffffffffffff", 16)
      dut.io.sources #= sourceIO
      assertIP(sourceIO, masterconfigs)
      assertIP(sourceIO, slave1configs)
      assertIP(sourceIO, slave2configs)

      dut.clockDomain.waitRisingEdge(10)

      sourceIO = BigInt("0", 16)
      dut.io.sources #= sourceIO
      assertIP(sourceIO, masterconfigs)
      assertIP(sourceIO, slave1configs)
      assertIP(sourceIO, slave2configs)

      dut.clockDomain.waitRisingEdge(100)
    }
  }
}

class APlicSystemTest extends APlicTest {
  var compile: SimCompiled[APlicSystemFiberTest] = null

  val sourcenum = 64
  val hartnum = 8

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slave1sourceIds = (1 to 63).toIndexedSeq
  val slave2sourceIds = (1 to 32).toIndexedSeq
  val hartIds = for (i <- 0 until hartnum) yield i

  def doCompile(): Unit ={
    compile = SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicSystemFiberTest(hartIds, sourceIds, slave1sourceIds, slave2sourceIds)
    )
  }

  test("system") {
    doCompile()

    compile.doSim("system"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0
      dut.io.ie.map(_ #= BigInt("7fffffffffffffff", 16))

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      val imsicAddr = 0x40000000
      val masterAddr = 0x30000000
      val slave1Addr = 0x20000000
      val slave2Addr = 0x10000000

      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))
      agent.putFullData(0, slave1Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000000))
      agent.putFullData(0, slave2Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000004))

      // M: 1 to 63
      // S1: 1 to 32

      val candidatesFull = (1 to 63).toSet
      val shuffledMCandidates = Random.shuffle(candidatesFull.toList).take(50)
      val shuffledS1Candidates = (1 to 32).toSet

      val masterconfigs = ArrayBuffer[gateway]()
      for (i <- 1 until sourcenum) {
        val isDelegaton = shuffledMCandidates.contains(i)
        val mode = if (isDelegaton) sourceMode.INACTIVE else sourceMode.random()
        val config = createGateway(mode, i, agent, masterAddr)
        config.hartId = 0
        config.iprio = 1
        config.setMode(agent, masterAddr, (i-1)*4, (if (isDelegaton) 1 else 0))
        masterconfigs += config
      }

      val slave1configs = ArrayBuffer[gateway]()
      val slave2configs = ArrayBuffer[gateway]()
      for (i <- candidatesFull) {
        var MDelegation = shuffledMCandidates.contains(i)
        var S1Delegation = shuffledS1Candidates.contains(i)
        var mode = if (MDelegation && !S1Delegation) sourceMode.random() else sourceMode.INACTIVE
        val slave1config = createGateway(mode, i, agent, slave1Addr)
        slave1config.hartId = Random.between(1, hartnum)
        slave1config.iprio = 1
        slave1config.setMode(agent, slave1Addr, (i-1)*4, (if (S1Delegation) 1 else 0))
        slave1configs += slave1config

        mode = if (S1Delegation && MDelegation) sourceMode.EDGE0 else sourceMode.INACTIVE
        val slave2config = createGateway(mode, i, agent, slave2Addr)
        slave2config.hartId = Random.between(1, hartnum)
        slave2config.deliveryMode = true
        slave2config.setMode(agent, slave2Addr, (i-1)*4, 0)
        slave2configs += slave2config
      }

      for (i <- 0 until hartnum) {
        agent.putFullData(0, masterAddr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))
        agent.putFullData(0, slave1Addr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))
        agent.putFullData(0, slave2Addr + aplicmap.idcOffset + aplicmap.ideliveryOffset + i * aplicmap.idcGroupSize, SimUInt32(0x1))
      }

      agent.putFullData(0, masterAddr + aplicmap.mmsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
      agent.putFullData(0, masterAddr + aplicmap.mmsiaddrcfghOffset, SimUInt32(0x3000))
      agent.putFullData(0, masterAddr + aplicmap.smsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
      agent.putFullData(0, masterAddr + aplicmap.smsiaddrcfghOffset, SimUInt32(0x0))

      agent.putFullData(0, masterAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
      agent.putFullData(0, slave1Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000100))
      agent.putFullData(0, slave2Addr + aplicmap.domaincfgOffset, SimUInt32(0x80000104))

      dut.clockDomain.waitRisingEdge(10)

      var sourceIO = BigInt("0", 16)
      dut.io.sources #= sourceIO
      assertIP(sourceIO, masterconfigs)
      assertIP(sourceIO, slave1configs)
      dut.clockDomain.waitRisingEdge(10)
      dut.clockDomain.waitRisingEdge(100)

      sourceIO = BigInt("7fffffffffffffff", 16)
      dut.io.sources #= sourceIO
      assertIP(sourceIO, masterconfigs)
      assertIP(sourceIO, slave1configs)
      dut.clockDomain.waitRisingEdge(100)

      var ipIO = BigInt("0", 16)
      var randomHartid = 0
      for (config <- slave2configs) {
        // 4.5.16
        if (config.mode != sourceMode.INACTIVE) {
          dut.io.sources #= sourceIO & ~((BigInt(1) << (config.idx-1)))
          dut.clockDomain.waitRisingEdge(4)
          ipIO = dut.io.ip(config.hartId-1).toBigInt
          assertIO(ipIO, (config.idx-1), 1, s"assert gateway ip output_${(config.idx-1)}")

          // wait busy bit 4.5.15
          Iterator
            .continually((agent.get(0, slave2Addr + aplicmap.genmsiOffset, 4).data(1) & 0x10) != 0)
            .takeWhile(identity)
            .foreach{_ => }

          randomHartid = Random.between(1, hartnum)
          agent.putFullData(0, slave2Addr + aplicmap.genmsiOffset, SimUInt32(randomHartid << 18 | ((config.idx-1)+1)))
          dut.clockDomain.waitRisingEdge(2)

          ipIO = dut.io.ip(randomHartid-1).toBigInt
          assertIO(ipIO, (config.idx-1), 1, s"assert genmsi ip output_${(config.idx-1)}")
        }
      }

      dut.clockDomain.waitRisingEdge(100)
    }
  }
}

class APlicTest extends SpinalSimFunSuite {
  onlyVerilator()

  val aplicmap = APlicMapping

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

  def assertIO(io: BigInt, id: Int, value: Int, name: String = "") = {
    assert(((io >> id) & BigInt(1)) == value, s"$name: missmatch value = ${io} >> ($id) & 1 != (${value})")
  }

  def assertIP(sourceIO: BigInt, configs: ArrayBuffer[gateway]) = {
    for (config <- configs) {
      if (config.mode != sourceMode.DETACHED) {
        config.assertIP(sourceIO.testBit(config.idx-1).toInt)
      }
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

    def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, childId: Int = 0): Unit
    def assertIE(): Unit
    def assertIP(io: Int): Unit
  }

  case class inactive(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.INACTIVE

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, childId: Int = 0) = {
      ie = 0
      if (childId == 0)
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x0))
      else
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400 | childId))
      agent.putFullData(0, base + aplicmap.clrienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie, s"ie: mode: inactive , id: $id")
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip, s"ip: mode: inactive, id: $id")
    }
  }

  case class detached(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.DETACHED

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, childId: Int = 0) = {
      ie = 1
      if (childId == 0)
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x1))
      else
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400 | childId))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie, s"ie: mode: detached, id: $id")
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip, s"ip: mode: detached, id: $id")
    }
  }

  case class edge1(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.EDGE1
    var reg = 0

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, childId: Int = 0) = {
      ie = 1
      if (childId == 0)
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x4))
      else
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400 | childId))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
        val offset = id / 32 * 4
        assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie, s"ie: mode: edge1, id: $id")
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      if (reg == 0 && io == 1) {
        ip = 1
      }
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip, s"ip: mode: edge1, id: $id")
      reg = io
    }
  }

  case class edge0(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.EDGE0
    var reg = 0

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, childId: Int = 0) = {
      ie = 1
      if (childId == 0)
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x5))
      else
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400 | childId))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie, s"ie: mode: edge0, id: $id")
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      if (reg == 1 && io == 0) {
        ip = 1
      }
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, ip, s"ip: mode: edge0, id: $id")
      reg = io
    }
  }

  case class level1(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.LEVEL1

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, childId: Int = 0) = {
      ie = 1
      if (childId == 0)
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x6))
      else
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400 | childId))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie, s"ie: mode: level1, id: $id")
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, io, s"ip: mode: level1, id: $id")
    }
  }

  case class level0(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends gateway(id) {
    mode = sourceMode.LEVEL0

    override def setMode(agent: tilelink.sim.MasterAgent, base: Int, offset: Int, childId: Int = 0) = {
      ie = 1
      if (childId == 0)
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x7))
      else
        agent.putFullData(0, base + aplicmap.sourcecfgOffset + offset, SimUInt32(0x400 | childId))
      agent.putFullData(0, base + aplicmap.setienumOffset, SimUInt32(id))
      agent.putFullData(0, base + aplicmap.targetOffset + offset, target)
    }
    override def assertIE() = {
      val offset = id / 32 * 4
      assertBit(agent.get(0, base + aplicmap.setieOffset + offset, 4), id, ie, s"ie: mode: level0, id: $id")
    }
    override def assertIP(io: Int) = {
      val offset = id / 32 * 4
      if (io == 0) {
        assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, 1, s"ip: mode: level0, id: $id")
      } else {
        assertBit(agent.get(0, base + aplicmap.setipOffset + offset, 4), id, 0, s"ip: mode: level0, id: $id")
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
