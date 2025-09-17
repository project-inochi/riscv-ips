package aia

import spinal.core._
import spinal.lib._

sealed trait InterruptMode extends AreaObject
object EDGE_RISING extends InterruptMode
object EDGE_FALLING extends InterruptMode
object LEVEL_HIGH extends InterruptMode
object LEVEL_LOWEL extends InterruptMode
object SPURIOUS extends InterruptMode

