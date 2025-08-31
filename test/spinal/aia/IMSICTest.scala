package aia

import scala.util.Random
import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.tester.SpinalSimFunSuite
import _root_.sim._

case class TilelinkImsicTest(hartIds: Seq[Int], sourceIds: Seq[Int], guestIds: Seq[Int], p: tilelink.M2sParameters) extends Component {
  val busP = p.toNodeParameters().toBusParameter()

  val blocks = hartIds.map(hartId => for (guestId <- guestIds) yield new SxAIABlock(sourceIds, hartId, guestId))

  val imsic = TilelinkImsicTrigger(blocks.flatMap(_.map(_.asImsicInfo())), ImsicMapping(), busP)
  val triggers = for ((block, trigger) <- blocks.flatMap(_.toSeq).zip(imsic.io.triggers)) yield new SxAIABlockTrigger(block, trigger)

  val io = new Bundle {
    val bus = slave(tilelink.Bus(busP))
    val ip = out Vec(blocks.map(hartBlock => Vec(hartBlock.map(block => Bits(block.interrupts.size bits)))))
    val profile = Vec(blocks.map(hartBlock => Vec(
      hartBlock.map(block => Vec(block.interrupts.map(int => new Bundle {
        val latency = out cloneOf(int.profile.latency)
        val en = in Bool()
      })))
    )))
  }

  io.bus <> imsic.io.bus
  io.ip := Vec(blocks.map(hartBlock => Vec(hartBlock.map(block => block.interrupts.map(_.ip).asBits()))))
  blocks.zip(io.profile).flatMap{ case (block, profile) => block.zip(profile)}.foreach({
    case (block, profs) => block.interrupts.zip(profs).foreach{ case (int, prof) => {
      int.profile.en := prof.en
      prof.latency := int.profile.latency
    }}
  })
}

class ImsicTest extends SpinalSimFunSuite {
  onlyVerilator()

  val hartNum = 8
  val sourceNum = 64
  val guestNum = 2
  val hartIds = for (i <- 0 until hartNum) yield i
  val sourceIds = for (i <- 1 until sourceNum) yield i
  val guestIds = for (i <- 0 to guestNum) yield i

  val infos = (for {hartId <- hartIds; guestId <- guestIds} yield ImsicInfo(
      hartId = hartId,
      guestId = guestId,
      sourceIds = sourceIds,
      groupId = 0,
      groupHartId = hartId,
    )).toArray.toSeq

  val mapping = ImsicMapping()
  val tilelinkBusP = tilelink.M2sParameters(
    sourceCount = 1,
    support = TilelinkImsicTrigger.getTilelinkSupport(
      transfers = tilelink.M2sTransfers(
        get = tilelink.SizeRange(1, 8),
        putFull = tilelink.SizeRange(1, 8)
      ),
      mapping, infos
    )
  )

  test("compile") {

    SimConfig.withConfig(config.TestConfig.spinal).compile(
      new TilelinkImsicTrigger(
        infos, mapping,
        tilelinkBusP.toNodeParameters().toBusParameter()
      )
    )
  }

  var compiled: SimCompiled[TilelinkImsicTest] = null
  val rndTestCase = 4000

  def doCompile() = {
    SimConfig.withConfig(config.TestConfig.spinal).withFstWave.compile(
      new TilelinkImsicTest(
        hartIds, sourceIds, guestIds, tilelinkBusP
      )
    )
  }

  def testInit(dut: TilelinkImsicTest): Unit = {
    for (hartId <- hartIds) {
      for (guestId <- guestIds) {
        for (sourceId <- sourceIds) {
          // dut.blocks(hartId)(guestId).interrupts(sourceId).profile.en #= false
          dut.io.profile(hartId)(guestId)(sourceId - 1).en #= false
        }
      }
    }

    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitRisingEdge()
  }

  test("set-test") {
    val compiled = doCompile()

    compiled.doSim("set-test") { dut =>
      testInit(dut)

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      val realMapping = dut.imsic.logic.realMapping
      println(s"mapping value: ${realMapping}")

      val testCases = for (i <- 0 until rndTestCase)
        yield (Random.nextInt(hartNum), Random.nextInt(sourceNum), Random.nextInt(guestNum + 1))

      for ((hartId, sourceId, guestId) <- testCases) {
        agent.putFullData(0, ImsicTrigger.imsicOffset(realMapping, 0, hartId, guestId).toLong, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge()

        if (sourceId != 0) {
          assert(dut.io.ip(hartId)(guestId).toBigInt.testBit(sourceId - 1), s"Fail on hart ${hartId}, guest ${guestId} interrupt ${sourceId}")
        }
      }
    }
  }

  test("set-test-clear") {
    val compiled = doCompile()

    compiled.doSim("set-test-clear") { dut =>
      testInit(dut)

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      val realMapping = dut.imsic.logic.realMapping
      println(s"mapping value: ${realMapping}")

      val testCases = for (i <- 0 until rndTestCase)
        yield (Random.nextInt(hartNum), Random.nextInt(sourceNum), Random.nextInt(guestNum + 1))

      for ((hartId, sourceId, guestId) <- testCases) {
        if (sourceId != 0) {
          assert(!dut.io.ip(hartId)(guestId).toBigInt.testBit(sourceId - 1), s"Interrupt ${sourceId} of hart ${hartId}, guest ${guestId} is not clear")
        }

        agent.putFullData(0, ImsicTrigger.imsicOffset(realMapping, 0, hartId, guestId).toLong, SimUInt32(sourceId))
        dut.clockDomain.waitRisingEdge(5)

        if (sourceId != 0) {
          assert(dut.io.ip(hartId)(guestId).toBigInt.testBit(sourceId - 1), s"Fail on hart ${hartId}, guest ${guestId} interrupt ${sourceId}")
          dut.blocks(hartId)(guestId).interrupts(sourceId - 1).ip #= false
          dut.clockDomain.waitRisingEdge()
        }
      }
    }
  }

  test("latency") {
    val compiled = doCompile()

    compiled.doSim("latency")(dut => {
      testInit(dut)

      implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
      val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

      val realMapping = dut.imsic.logic.realMapping
      println(s"mapping value: ${realMapping}")

      for (hartId <- hartIds) {
        for (sourceId <- sourceIds) {
          dut.io.profile(hartId)(0)(sourceId - 1).en #= true
          agent.putFullData(0, ImsicTrigger.imsicOffset(realMapping, 0, hartId, 0).toLong, SimUInt32(sourceId))
        }
      }
      dut.clockDomain.waitRisingEdge()

      val maxLatency = dut.io.profile.flatMap(_(0).map(_.latency.toBigInt)).max
      assert(maxLatency < 10, s"Its max delay seems to be too slow")
      println(s"Test max latency is ${maxLatency}")
    })
  }
}
