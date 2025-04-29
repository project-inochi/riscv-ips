package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import _root_.sim._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

case class APlicDelegateTestFiber(hartIds: Seq[Int], sourceIds: Seq[Int], slavesourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkAPlicReadPortFiber()

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

class APlicMSTest extends APlicTest {
  val sourcenum = 64
  val hartnum = 8

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = (1 to 63).toIndexedSeq
  val hartIds = for (i <- 0 until hartnum) yield i

  test("MS Direct") {
    SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicDelegateTestFiber(hartIds, sourceIds, slavesourceIds)
    ).doSim("MS Direct"){ dut =>
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
