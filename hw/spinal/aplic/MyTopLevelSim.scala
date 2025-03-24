package aplic

import spinal.core._
import spinal.core.sim._

object MyTopLevelSim extends App {
  Config.sim.compile(aplic()).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    var modelState = 0
    for (idx <- 0 to 99) {
      // Drive the dut inputs with random values

      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()
    }
  }
}
