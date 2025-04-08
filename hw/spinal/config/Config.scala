package config

import spinal.core._
import spinal.core.sim._

object Config {
  def spinal = SpinalConfig(
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
