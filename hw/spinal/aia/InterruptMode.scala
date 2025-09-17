package aia

import spinal.core._
import spinal.lib._
import spinal.lib.misc.InterruptNode
import spinal.lib.misc.plic

sealed trait InterruptMode
object EDGE_RISING extends InterruptMode
object EDGE_FALLING extends InterruptMode
object LEVEL_HIGH extends InterruptMode
object LEVEL_LOW extends InterruptMode
object SPURIOUS extends InterruptMode

trait InterruptCtrlFiber extends plic.InterruptCtrlFiber {
  def defaultInterruptMode: InterruptMode
  def createInterruptSlave(id: Int, mode: InterruptMode): InterruptNode

  def mapUpInterrupt(id: Int, node: InterruptNode, mode: InterruptMode): Unit = {
    val local = createInterruptSlave(id, mode)
    local.setLambdaName(node.isNamed && this.isNamed)(s"${this.getName()}_from_${node.getName}")
    local << node
    mappedInterrupts(node) = local
  }

  override def createInterruptSlave(id: Int): InterruptNode = createInterruptSlave(id, defaultInterruptMode)
}
