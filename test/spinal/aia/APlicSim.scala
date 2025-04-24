package aia

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester, SpinalSimFunSuite}
import _root_.sim._

case class TilelinkAPLICFiberTest(hartIds: Seq[Int], sourceIds: Seq[Int], slavesourceIds: Seq[Int]) extends Component {
  val masterBus = TilelinkBusFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val blocks = for (hartId <- hartIds) yield new SxAIABlock(sourceIds, hartId, 0)

  val slaveInfos = Seq(APlicSlaveInfo(1, slavesourceIds))

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val S = TilelinkAPLICFiber()
    S.up at 0x10000000 of access
    crossBar << S.down

    val M = TilelinkAPLICFiber()
    M.up at 0x20000000 of access
    crossBar << M.down

    val dispatcher = TilelinkIMSICFiber()
    dispatcher.node at 0x30000000 of access

    M.domainParam = Some(new APlicDomainParam(true, true, APlicGenParam.MSI))
    S.domainParam = Some(new APlicDomainParam(false, false, APlicGenParam.MSI))

    for (block <- blocks) {
      val trigger = dispatcher.addIMSICinfo(block.asTilelinkIMSICIInfo())
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

    val sourcesMBundles = sourceIds.map(sourceId => {
      val node = InterruptNode.master()
      M.mapUpInterrupt(sourceId, node)
      node
    })

    val slaveSources = slaveInfos.map(M.createInterruptDelegation(_))

    // XXX: there is only one slave
    val sourcesSBundles = slavesourceIds.zip(slaveSources(0).flags).map {
      case (id, slaveSource) => S.mapUpInterrupt(id, slaveSource)
    }

    S.mmsiaddrcfg := M.mmsiaddrcfg
    S.smsiaddrcfg := M.smsiaddrcfg
  }

  val io = new Bundle {
    val bus = slave(tilelink.Bus(masterBus.m2sParams.toNodeParameters().toBusParameter()))
    val sources = in Bits(sourceIds.size bits)
    val ie = in Vec(blocks.map(block => Bits(block.interrupts.size bits)))
    val ip = out Vec(blocks.map(block => Bits(block.interrupts.size bits)))
    val targetsmaster = out Bits(hartIds.size bits)
    val targetsslave = out Bits(hartIds.size bits)
  }

  masterBus.bus = Some(io.bus)

  peripherals.sourcesMBundles.lazyZip(io.sources.asBools).foreach(_.flag := _)

  io.targetsmaster := peripherals.targetsMBundles.map(_.flag).asBits()
  io.targetsslave := peripherals.targetsSBundles.map(_.flag).asBits()

  Vec(blocks.map(block => block.interrupts.map(_.ie).asBits())) := io.ie
  io.ip := Vec(blocks.map(block => block.interrupts.map(_.ip).asBits()))
}

class APlicSimTest extends SpinalSimFunSuite {
  onlyVerilator()

  val sourcenum = 64
  val hartnum = 8

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = IndexedSeq(1, 4)
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping

  var compiled: SimCompiled[TilelinkAPLICFiberTest] = null

  def doCompile(): Unit = {
    compiled = SimConfig.withConfig(config.TestConfig.spinal).compile(
      new TilelinkAPLICFiberTest(hartIds, sourceIds, slavesourceIds)
    )
  }

  def assertData(data: tilelink.sim.TransactionD, answer: Int, name: String): Unit = {
    val value = data.data
    val result = value.zip(answer.toBytes).forall { case (x, y) => x == y }
    assert(result, s"$name: missmatch (${value.toList} != 0x${answer.toBytes.slice(0, 4)})")
  }

  test("compile") {
    doCompile()
  }

  test("aplic sim direct") {
    if(compiled == null) {
      doCompile()
    }

    compiled.doSimUntilVoid("aplic sim direct") { dut =>
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
      for (i <- 0 until prioArrayM.size) {
        print(agent.putFullData(0, masteroffset + aplicmap.sourcecfgOffset + i * 4, prioArrayM(i)))
        print(agent.putFullData(0, slaveoffset + aplicmap.sourcecfgOffset + i * 4, prioArrayS(i)))
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
      for (i <- 0 until targetArray.size) {
        print(agent.putFullData(0, masteroffset + aplicmap.targetOffset + i * 4, targetArray(i)))
        print(agent.putFullData(0, slaveoffset + aplicmap.targetOffset + i * 4, targetArray(i)))
      }

      for (i <- 0 until hartnum) {
        print(agent.putFullData(0, masteroffset + aplicmap.idcOffset + aplicmap.idcGroupSize * i + aplicmap.ideliveryOffset, SimUInt32(1)))
        print(agent.putFullData(0, slaveoffset + aplicmap.idcOffset + aplicmap.idcGroupSize * i + aplicmap.ideliveryOffset, SimUInt32(1)))
      }

      print(agent.putFullData(0, masteroffset + aplicmap.setipOffset, SimUInt32(0x0)))
      print(agent.putFullData(0, slaveoffset + aplicmap.setipOffset, SimUInt32(0x0)))

      print(agent.putFullData(0, masteroffset + aplicmap.domaincfgOffset, SimUInt32(0x80000100)))
      print(agent.putFullData(0, slaveoffset + aplicmap.domaincfgOffset, SimUInt32(0x80000100)))

      // set/clripnum
      print(agent.putFullData(0, masteroffset + aplicmap.clrieOffset, SimUInt32(0b100)))
      assertData(agent.get(0, masteroffset + aplicmap.setieOffset, 4), 0xe8, "master_ip")
      print(agent.putFullData(0, masteroffset + aplicmap.setieOffset, SimUInt32(0xff)))

      print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x2)))
      dut.clockDomain.waitRisingEdge(10)

      val aplicmaster = dut.peripherals.M.core.aplic
      val aplicslave = dut.peripherals.S.core.aplic

      assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000004, "master_ip")
      print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x1)))
      assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000004, "master_ip")

      print(agent.putFullData(0, masteroffset + aplicmap.in_clripOffset, SimUInt32(0b100)))
      assertData(agent.get(0, masteroffset + aplicmap.setipOffset, 4), 0x00000000, "master_ip")

      print(agent.putFullData(0, masteroffset + aplicmap.setipnumOffset, SimUInt32(0x2)))

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
      assertData(agent.get(0, masteroffset + aplicmap.in_clripOffset, 4), 0x000000c0, "master_ip")
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
      simSuccess()
    }
  }

  test("aplic sim msi") {
    if(compiled == null) {
      println("rebuild")
      doCompile()
    }

    compiled.doSimUntilVoid("aplic sim msi") { dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0b0000000
      dut.io.ie(0) #= 0x7f

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      val slaveoffset = 0x10000000
      val masteroffset = 0x20000000
      val imsicoffset = 0x30000000

      print(agent.putFullData(0, masteroffset + aplicmap.domaincfgOffset, SimUInt32(0x80000004)))
      print(agent.putFullData(0, slaveoffset + aplicmap.domaincfgOffset, SimUInt32(0x80000004)))

      for (i <- 1 to 6) {
        print(agent.putFullData(0, masteroffset + aplicmap.sourcecfgOffset + (i - 1) * 4, SimUInt32(0x6)))
        print(agent.putFullData(0, masteroffset + aplicmap.targetOffset + (i - 1) * 4, SimUInt32(i)))
        print(agent.putFullData(0, masteroffset + aplicmap.setienumOffset, SimUInt32(i)))

        print(agent.putFullData(0, slaveoffset + aplicmap.sourcecfgOffset + (i - 1) * 4, SimUInt32(0x6)))
        print(agent.putFullData(0, slaveoffset + aplicmap.targetOffset + (i - 1) * 4, SimUInt32(i)))
        print(agent.putFullData(0, slaveoffset + aplicmap.setienumOffset, SimUInt32(i)))
      }

      print(agent.putFullData(0, masteroffset + aplicmap.mmsiaddrcfgOffset, SimUInt32(imsicoffset>>12)))
      print(agent.putFullData(0, masteroffset + aplicmap.mmsiaddrcfghOffset, SimUInt32(0x1000)))
      print(agent.putFullData(0, masteroffset + aplicmap.smsiaddrcfgOffset, SimUInt32(imsicoffset>>12)))
      print(agent.putFullData(0, masteroffset + aplicmap.smsiaddrcfghOffset, SimUInt32(0x0)))

      print(agent.putFullData(0, masteroffset + aplicmap.domaincfgOffset, SimUInt32(0x80000104)))
      print(agent.putFullData(0, slaveoffset + aplicmap.domaincfgOffset, SimUInt32(0x80000104)))

      dut.io.sources #= 0b0011111

      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x7)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x6)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x5)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x4)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x3)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x2)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x1)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x2)))
      print(agent.putFullData(0, masteroffset + aplicmap.genmsiOffset, SimUInt32(0x40004)))
      print(agent.putFullData(0, slaveoffset + aplicmap.genmsiOffset, SimUInt32(0x7)))
      print(agent.putFullData(0, slaveoffset + aplicmap.genmsiOffset, SimUInt32(0x40001)))

      dut.clockDomain.waitRisingEdge(50)
      simSuccess()
    }
  }
  // }
}
