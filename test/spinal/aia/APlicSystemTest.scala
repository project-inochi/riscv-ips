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

  val slave1Infos = Seq(APlicChildInfo(1, slave1sourceIds))
  val slave2Infos = Seq(APlicChildInfo(1, slave2sourceIds))

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val S = TilelinkAPLICFiber(APlicDomainParam.S(APlicGenParam.full))
    S.node at 0x10000000 of access

    val S_Sender = TilelinkAPLICMSISenderFiber()
    crossBar << S_Sender.node

    S_Sender.createMSIStreamConsumer() << S.createMSIStreamProducer()

    val M = TilelinkAPLICFiber(APlicDomainParam.M(APlicGenParam.full))
    M.node at 0x20000000 of access

    val M_Sender = TilelinkAPLICMSISenderFiber()
    crossBar << M_Sender.node

    M_Sender.createMSIStreamConsumer() << M.createMSIStreamProducer()

    val ROOT = TilelinkAPLICFiber(APlicDomainParam.root(APlicGenParam.direct.withMSIAddrCfg()))
    ROOT.node at 0x30000000 of access

    val dispatcher = TilelinkImsicTriggerFiber()
    dispatcher.node at 0x40000000 of access

    for (block <- blocks) {
      val trigger = dispatcher.addImsicinfo(block.asTilelinkImsicInfo())
      val connector = SxAIABlockTrigger(block, trigger)
    }

    val targetsSBundles = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      S.mapDownInterrupt(hartId, node)
      node
    })

    val targetsMBundles = hartIds.map(hartId => {
      val node = InterruptNode.slave()
      M.mapDownInterrupt(hartId, node)
      node
    })

    val targetsRootBundles = InterruptNode.slave()
    ROOT.mapDownInterrupt(0, targetsRootBundles)

    val sourcesMBundles = sourceIds.map(sourceId => {
      val node = InterruptNode.master()
      ROOT.mapUpInterrupt(sourceId, node)
      node
    })

    val slaveMSources = slave1Infos.map(ROOT.createInterruptDelegation(_))
    val slaveS1Sources = slave2Infos.map(M.createInterruptDelegation(_))

    val sourcesS2Bundles = slave2sourceIds.zip(slaveS1Sources(0).flags).map {
      case (id, childSource) => S.mapUpInterrupt(id, childSource)
    }

    val sourcesS1Bundles = slave1sourceIds.zip(slaveMSources(0).flags).map {
      case (id, childSource) => M.mapUpInterrupt(id, childSource)
    }

    M.mmsiaddrcfg := ROOT.mmsiaddrcfg
    M.smsiaddrcfg := ROOT.smsiaddrcfg
    S.mmsiaddrcfg := ROOT.mmsiaddrcfg
    S.smsiaddrcfg := ROOT.smsiaddrcfg
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

  for ((bundle, source) <- peripherals.sourcesMBundles.zip(io.sources.asBools)) {
    bundle.flag := source
  }

  io.targetsmaster := peripherals.targetsRootBundles.flag.asBits
  io.targets1slave := peripherals.targetsMBundles.map(_.flag).asBits()
  io.targets2slave := peripherals.targetsSBundles.map(_.flag).asBits()

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

      val masterconfigs = ArrayBuffer[APlicSimSource]()
      for (i <- 1 until sourcenum) {
        val isDelegaton = shuffledMCandidates.contains(i)
        val mode = if (isDelegaton) APlicSimSourceMode.INACTIVE else APlicSimSourceMode.random()
        val config = createGateway(mode, i, agent, masterAddr)
        config.hartId = 0
        config.iprio = 1
        config.setMode(agent, masterAddr, (i-1)*4, (if (isDelegaton) 1 else 0))
        masterconfigs += config
      }

      val slave1configs = ArrayBuffer[APlicSimSource]()
      val slave2configs = ArrayBuffer[APlicSimSource]()
      for (i <- candidatesFull) {
        var MDelegation = shuffledMCandidates.contains(i)
        var S1Delegation = shuffledS1Candidates.contains(i)
        var mode = if (MDelegation && !S1Delegation) APlicSimSourceMode.random() else APlicSimSourceMode.INACTIVE
        val slave1config = createGateway(mode, i, agent, slave1Addr)
        slave1config.hartId = Random.between(1, hartnum)
        slave1config.iprio = 1
        slave1config.setMode(agent, slave1Addr, (i-1)*4, (if (S1Delegation) 1 else 0))
        slave1configs += slave1config

        mode = if (S1Delegation && MDelegation) APlicSimSourceMode.EDGE0 else APlicSimSourceMode.INACTIVE
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
        if (config.mode != APlicSimSourceMode.INACTIVE) {
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
