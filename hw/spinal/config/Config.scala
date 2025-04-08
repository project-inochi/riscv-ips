package config

import spinal.core._
import spinal.core.sim._

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    ),
    onlyStdLogicVectorAtTopLevelIo = false,
    removePruned = true,
    genLineComments = true,
    verbose = true,
  )

  def sim = SimConfig.withConfig(spinal).withFstWave
}
