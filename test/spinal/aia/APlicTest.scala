package aia

import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}
import _root_.sim._

class APlicTest extends SpinalAnyFunSuite {
  var compile: SimCompiled[TilelinkAPLICFiberTest] = null

  val sourcenum = 8
  val hartnum = 2

  val sourceIds = for (i <- 1 until sourcenum) yield i
  val slavesourceIds = IndexedSeq(1, 4)
  val hartIds = for (i <- 0 until hartnum) yield i

  val aplicmap = APlicMapping

  def doCompile(): Unit ={
    compile = config.TestConfig.sim.compile(
      new TilelinkAPLICFiberTest(hartIds, sourceIds, slavesourceIds)
    )
  }

  test("compile") {
    doCompile()
  }
}
