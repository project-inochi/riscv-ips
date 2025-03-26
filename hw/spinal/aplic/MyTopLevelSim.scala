package aplic

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import spinal.core

object aplicSim extends App {

  val sourcenum = 8
  val hartnum = 8

  val compile = Config.sim.compile{
    val imsic = new TilelinkAplic(sourcenum, hartnum,
      tilelink.M2sParameters(
        sourceCount = 1,
        support = tilelink.M2sSupport(
          addressWidth = 16,
          dataWidth = 32,
          transfers = tilelink.M2sTransfers(
            get = tilelink.SizeRange(8),
            putFull = tilelink.SizeRange(8)
          )
        )
      ).toNodeParameters().toBusParameter()
    )

    imsic
  }

  compile.doSim{ dut =>
    dut.clockDomain.forkStimulus(10)

    implicit val idAllocator = new tilelink.sim.IdAllocator(tilelink.DebugId.width)
    val agent = new tilelink.sim.MasterAgent(dut.io.bus, dut.clockDomain)

    val domaincfgdata = BigInt(0x80000100).toByteArray
    print(agent.putFullData(0, 0x0, domaincfgdata))

    val sourcecfgdata = BigInt(0x6).toByteArray
    print(agent.putFullData(0, 0x6, domaincfgdata))


  }




  // Config.sim.compile(aplic()).doSim { dut =>
  //   // Fork a process to generate the reset and the clock on the dut
  //   dut.clockDomain.forkStimulus(period = 10)

  //   var modelState = 0
  //   for (idx <- 0 to 99) {
  //     // Drive the dut inputs with random values

  //     // Wait a rising edge on the clock
  //     dut.clockDomain.waitRisingEdge()
  //   }
  // }
}
