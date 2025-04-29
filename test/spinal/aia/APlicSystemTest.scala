package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import spinal.tester.SpinalSimFunSuite
import _root_.sim._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

case class APlicSystemTestFiber(hartIds: Seq[Int], sourceIds: Seq[Int], slave1sourceIds: Seq[Int], slave2sourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkAPlicReadPortFiber()

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

    M.domainParam = Some(APlicDomainParam.root(APlicGenParam.direct.withMSIAddrCfg()))
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

class APlicSystemTest extends SpinalSimFunSuite {
  onlyVerilator()

  import APlicTestHelper._

  val sourcenum = 64
  val hartnum = 8

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slave1sourceIds = (1 to 63).toIndexedSeq
  val slave2sourceIds = (1 to 32).toIndexedSeq
  val hartIds = for (i <- 0 until hartnum) yield i

  test("system") {
    SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicSystemTestFiber(hartIds, sourceIds, slave1sourceIds, slave2sourceIds)
    ).doSim("system"){ dut =>
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

      val masterconfigs = ArrayBuffer[APlicSource]()
      for (i <- 1 until sourcenum) {
        val isDelegaton = shuffledMCandidates.contains(i)
        val mode = if (isDelegaton) sourceMode.INACTIVE else sourceMode.random()
        val config = createGateway(mode, i, agent, masterAddr)
        config.hartId = 0
        config.iprio = 1
        config.setMode(agent, masterAddr, (i-1)*4, (if (isDelegaton) 1 else 0))
        masterconfigs += config
      }

      val slave1configs = ArrayBuffer[APlicSource]()
      val slave2configs = ArrayBuffer[APlicSource]()
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
