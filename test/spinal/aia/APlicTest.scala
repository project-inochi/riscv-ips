package aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.misc.InterruptNode
import spinal.tester.SpinalSimFunSuite
import _root_.sim._

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

case class TilelinkAPlicReadPortFiber() extends Area {
  val node = tilelink.fabric.Node.down()

  val m2sParams = tilelink.M2sParameters(
    addressWidth = 32,
    dataWidth = 64,
    masters = List(
      tilelink.M2sAgent(
        name = TilelinkAPlicReadPortFiber.this,
        mapping = List(
          tilelink.M2sSource(
            id = SizeMapping(0, 4),
            emits = tilelink.M2sTransfers(
              get = tilelink.SizeRange(1, 64),
              putFull = tilelink.SizeRange(1, 64)
            )
          )
        )
      )
    )
  )

  var bus: Option[tilelink.Bus] = None

  val fiber = Fiber build new Area {
    node.m2s forceParameters m2sParams

    node.s2m.supported load tilelink.S2mSupport.none()

    val mappings = spinal.lib.system.tag.MemoryConnection.getMemoryTransfers(node)
    for(mapping <- mappings){
      println(s"- ${mapping.where} -> ${mapping.transfers}")
    }

    bus.map(node.bus <> _)
  }
}

case class APlicUnitTestFiber(hartIds: Seq[Int], sourceIds: Seq[Int], guestIds: Seq[Int]) extends Component {
  val masterBus = TilelinkAPlicReadPortFiber()

  val crossBar = tilelink.fabric.Node()
  crossBar << masterBus.node

  val blocks = hartIds.map(hartId => for (guestId <- guestIds) yield new SxAIABlock(sourceIds, hartId, guestId))

  val APlicGenMode = APlicGenParam.test

  val peripherals = new Area {
    val access = tilelink.fabric.Node()
    access << crossBar

    val M = TilelinkAPLICFiber()
    M.up at 0x10000000 of access
    crossBar << M.down

    val dispatcher = TilelinkIMSICFiber()
    dispatcher.node at 0x30000000 of access

    for (hartBlock <- blocks) {
      for (block <- hartBlock) {
        val trigger = dispatcher.addIMSICinfo(block.asTilelinkIMSICIInfo())
        val connector = SxAIABlockTrigger(block, trigger)
      }
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
    val ie = in Vec(blocks.map(hartBlock => Vec(hartBlock.map(block => Bits(block.interrupts.size bits)))))
    val ip = out Vec(blocks.map(hartBlock => Vec(hartBlock.map(block => Bits(block.interrupts.size bits)))))
  }

  masterBus.bus = Some(io.bus)

  peripherals.sourcesMBundles.lazyZip(io.sources.asBools).foreach(_.flag := _)

  io.targetsmaster := peripherals.targetsMBundles.map(_.flag).asBits()

  io.ip := Vec(blocks.map(hartBlock => Vec(hartBlock.map(block => block.interrupts.map(_.ip).asBits()))))
  Vec(blocks.map(hartBlock => Vec(hartBlock.map(block => block.interrupts.map(_.ie).asBits())))) := io.ie

}

class APlicUnitTest extends SpinalSimFunSuite {
  onlyVerilator()

  import APlicTestHelper._

  val sourcenum = 64
  val hartnum = 8
  val guestNum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val hartIds = for (i <- 0 until hartnum) yield i
  val guestIds = for (i <- 0 to guestNum) yield i

  test("Direct") {
    SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicUnitTestFiber(hartIds, sourceIds, Seq(0))
    ).doSim("Direct"){ dut =>
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

      val configs = ArrayBuffer[APlicSource]()
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
    SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new APlicUnitTestFiber(hartIds, sourceIds, guestIds)
    ).doSim("MSI"){ dut =>
      dut.clockDomain.forkStimulus(10)

      dut.io.sources #= 0x0
      dut.io.ie.map(_.map(_ #= BigInt("7fffffffffffffff", 16)))

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      val aplicAddr = 0x10000000
      val imsicAddr = 0x30000000

      agent.putFullData(0, aplicAddr + aplicmap.domaincfgOffset, SimUInt32(0x80000004))

      val configs = ArrayBuffer[APlicSource]()
      for (i <- 1 until sourcenum) {
        val mode = sourceMode.EDGE1
        val config = createGateway(mode, i, agent, aplicAddr)
        config.hartId = Random.nextInt(hartnum)
        config.deliveryMode = true
        config.guestIndex = Random.nextInt(3)
        config.setMode(agent, aplicAddr, (i-1)*4)
        configs += config
      }

      agent.putFullData(0, aplicAddr + aplicmap.mmsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
      agent.putFullData(0, aplicAddr + aplicmap.mmsiaddrcfghOffset, SimUInt32(0x203000))
      agent.putFullData(0, aplicAddr + aplicmap.smsiaddrcfgOffset, SimUInt32(imsicAddr>>12))
      agent.putFullData(0, aplicAddr + aplicmap.smsiaddrcfghOffset, SimUInt32(0x200000))

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
        ipIO = dut.io.ip(config.hartId)(config.guestIndex).toBigInt
        assertIO(ipIO, i, 1, s"assert gateway ip output_$i")

        // wait busy bit 4.5.15
        Iterator
          .continually((agent.get(0, aplicAddr + aplicmap.genmsiOffset, 4).data(1) & 0x10) != 0)
          .takeWhile(identity)
          .foreach{_ => }

        randomHartid = Random.between(1, hartnum)
        agent.putFullData(0, aplicAddr + aplicmap.genmsiOffset, SimUInt32(randomHartid << 18 | i+1))
        dut.clockDomain.waitRisingEdge(2)

        ipIO = dut.io.ip(randomHartid)(0).toBigInt
        assertIO(ipIO, i, 1, s"assert genmsi ip output_$i")
      }

      // // 4.5.3 when lock
      agent.putFullData(0, aplicAddr + aplicmap.mmsiaddrcfghOffset, SimUInt32(0x80203000))
      assertData(agent.get(0, aplicAddr + aplicmap.mmsiaddrcfgOffset, 4), 0x0, "mmsiaddrcfgWithLock")
      assertData(agent.get(0, aplicAddr + aplicmap.mmsiaddrcfghOffset, 4), 0x80000000, "mmsiaddrcfghWithLock")
      assertData(agent.get(0, aplicAddr + aplicmap.smsiaddrcfgOffset, 4), 0x0, "smsiaddrcfgWithLock")
      assertData(agent.get(0, aplicAddr + aplicmap.smsiaddrcfghOffset, 4), 0x0, "smsiaddrcfghWithLock")


      dut.clockDomain.waitRisingEdge(100)
    }
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

object APlicTestHelper {
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

  def assertIP(sourceIO: BigInt, configs: ArrayBuffer[APlicSource]) = {
    for (config <- configs) {
      if (config.mode != sourceMode.DETACHED) {
        config.assertIP(sourceIO.testBit(config.idx-1).toInt)
      }
    }
  }

  def createGateway(mode: sourceMode.Value, id: Int, agent: tilelink.sim.MasterAgent, baseaddr: Int): APlicSource = {
    mode match {
      case sourceMode.INACTIVE => APlicInactiveSource(id, agent, baseaddr)
      case sourceMode.DETACHED => APlicDetachedSource(id, agent, baseaddr)
      case sourceMode.EDGE1    => APlicEdge1Source(id, agent, baseaddr)
      case sourceMode.EDGE0    => APlicEdge0Source(id, agent, baseaddr)
      case sourceMode.LEVEL1   => APlicLevel1Source(id, agent, baseaddr)
      case sourceMode.LEVEL0   => APlicLevel0Source(id, agent, baseaddr)
    }
  }
}

abstract class APlicSource(id: Int) {
  import APlicTestHelper._

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

case class APlicInactiveSource(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends APlicSource(id) {
  import APlicTestHelper._

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

case class APlicDetachedSource(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends APlicSource(id) {
  import APlicTestHelper._

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

case class APlicEdge1Source(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends APlicSource(id) {
  import APlicTestHelper._

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

case class APlicEdge0Source(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends APlicSource(id) {
  import APlicTestHelper._

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

case class APlicLevel1Source(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends APlicSource(id) {
  import APlicTestHelper._

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

case class APlicLevel0Source(id: Int, agent: tilelink.sim.MasterAgent, base: Int) extends APlicSource(id) {
  import APlicTestHelper._

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
