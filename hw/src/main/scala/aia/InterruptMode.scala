package aia

import spinal.core._
import spinal.lib._
import spinal.lib.misc._

trait CasInterruptCtrlFiber extends InterruptCtrlFiber {
  def defaultInterruptMode: InterruptMode
  def createInterruptSlave(id: Int, mode: InterruptMode): InterruptNode

  override def mapUpInterrupt(id: Int, node: InterruptNode, mode: InterruptMode): Unit = {
    val local = createInterruptSlave(id, mode)
    local.setLambdaName(node.isNamed && this.isNamed)(s"${this.getName()}_from_${node.getName}")
    local << node
    mappedInterrupts(node) = local
  }

  override def createInterruptSlave(id: Int): InterruptNode = createInterruptSlave(id, defaultInterruptMode)
}
