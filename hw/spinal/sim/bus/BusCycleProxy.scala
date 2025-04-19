package sim.bus

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc._
import spinal.lib.misc.slot.{Slot, SlotPool}

trait BusMasterSend {
  def send(address: UInt, data: UInt): Unit
}

class BusCycleProxy[TS <: spinal.core.Data with IMasterSlave,
										TM <: spinal.core.Data with IMasterSlave,
                    TH <: BusMasterSend](slaveType: HardType[TS],
                                         masterType: HardType[TM],
                                         factoryGen: TS => BusSlaveFactory,
                                         helperGen: TM => TH) extends Component {
  val io = new Bundle {
    val slaveBus = slave(slaveType())
    val masterBus = master(masterType())
  }

	val address = Reg(UInt(64 bits))

  val factory = factoryGen(io.slaveBus)
	factory.readAndWriteMultiWord(address, address = 0x0)

	val data = factory.createAndDriveFlow(UInt(32 bits), address = 0x8)

  val helper = helperGen(io.masterBus)

  when (data.valid) {
    helper.send(address, data.payload)
  }
}

case class TilelinkBusCycleProxy(slaveParams: tilelink.BusParameter, mastersParams: tilelink.BusParameter) extends BusCycleProxy(
  new bus.tilelink.Bus(slaveParams),
  new bus.tilelink.Bus(mastersParams),
  new bus.tilelink.SlaveFactory(_, true),
  new TilelinkMasterHelper(_)
)

case class TilelinkMasterHelper(bus: tilelink.Bus) extends Area with BusMasterSend {
  val busA = cloneOf(bus.a)
  val busD = bus.d

  bus.a <-< busA

  busD.ready := True
  busA.valid := False
  busA.payload.assignDontCare()

  def send(address: UInt, data: UInt) = {
    busA.valid    := True
    busA.opcode   := tilelink.Opcode.A.PUT_FULL_DATA
    busA.size     := 2
    busA.source   := 0
    busA.address  := address.resized
    busA.data     := data.asBits.resized
    busA.debugId  := 0
    busA.mask     := 0xf
  }
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
    dataWidth = 32,
    masters = List(
      tilelink.M2sAgent(
        name = name,
        mapping = List(
          tilelink.M2sSource(
            id = SizeMapping(0, 4),
            emits = tilelink.M2sTransfers(
              putFull = tilelink.SizeRange(4)
            )
          )
        )
      )
    )
  )
}
