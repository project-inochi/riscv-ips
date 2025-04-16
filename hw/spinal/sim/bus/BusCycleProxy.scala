package sim.bus

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc._
import spinal.lib.misc.slot.{Slot, SlotPool}

class BusCycleProxy[T1 <: spinal.core.Data with IMasterSlave,
										T2 <: spinal.core.Data with IMasterSlave](slaveType: HardType[T1],
										 																					masterType: HardType[T2],
                                                           		factoryGen: T1 => BusSlaveFactory) extends Component {
  val io = new Bundle {
    val slaveBus = slave(slaveType())
    val masterBus = master(masterType())
  }

	val address = Reg(UInt(64 bits))

  val factory = factoryGen(io.slaveBus)
	factory.readAndWriteMultiWord(address, address = 0x0)

	val data = factory.createAndDriveFlow(UInt(32 bits), address = 0x8)
}

case class TilelinkBusCycleProxy(slaveParams: tilelink.BusParameter, mastersParams: tilelink.BusParameter) extends BusCycleProxy(
  new bus.tilelink.Bus(slaveParams),
  new bus.tilelink.Bus(mastersParams),
  new bus.tilelink.SlaveFactory(_, true)
) {
  val busA = cloneOf(io.masterBus.a)
  val busD = io.masterBus.d

  io.masterBus.a <-< busA

  busD.ready := True
  busA.valid := False
  busA.payload.assignDontCare()

  when (data.valid) {
    busA.valid    := True
    busA.opcode   := tilelink.Opcode.A.PUT_FULL_DATA
    busA.size     := 4
    busA.source   := 0
    busA.address  := address.resized
    busA.data     := data.payload.asBits.resized
    busA.debugId  := 0
    busA.mask     := 0xf
  }
  /*
   * TODO:
   * 8 / 16 bits : counter + flag
   * busA.size busA.mask (source?)
   */
}

object TilelinkBusCycleProxy {
  def getTilelinkSlaveSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = 20,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )

  def getTilelinkMasterSupport(name: Nameable) = bus.tilelink.M2sParameters(
    addressWidth = 64,
    dataWidth = 64,
    masters = List(
      tilelink.M2sAgent(
        name = name,
        mapping = List(
          tilelink.M2sSource(
            id = SizeMapping(0, 4),
            emits = tilelink.M2sTransfers(
              get = tilelink.SizeRange(1, 64),
              putFull = tilelink.SizeRange(1, 64)
            )
          )
        )
      )
    )
  )
}
