package config

import spinal.core._
import spinal.core.sim._

object Config {
  def spinal = SpinalConfig(
    mode = SystemVerilog,
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    genLineComments = true,
    onlyStdLogicVectorAtTopLevelIo = false,
    oneFilePerComponent = true,
    removePruned = true,
    verbose = true,
  )

  def sim = SimConfig.withConfig(spinal).withFstWave
}

object TestConfig {
  def spinal = SpinalConfig(
    mode = SystemVerilog,
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    oneFilePerComponent = true,
    removePruned = true,
    verbose = true
  )
}
