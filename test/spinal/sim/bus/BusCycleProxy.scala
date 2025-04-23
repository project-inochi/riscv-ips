package sim.bus

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.misc._
import spinal.lib.misc.slot.{Slot, SlotPool}

case class BusMasterWritePayload() extends Bundle {
  val address = UInt(64 bits)
  val data = UInt(32 bits)
}

trait BusMasterSend {
  def send(stream: Stream[BusMasterWritePayload]): Area
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

	val dataFlow = factory.createAndDriveFlow(UInt(32 bits), address = 0x8)
  val (dataStream, fifoOccupancy) = dataFlow.queueWithOccupancy(2)
  val payloadStream = dataStream.map(buildWritePayload(address, _))

  val helper = helperGen(io.masterBus)
  helper.send(payloadStream)

  def buildWritePayload(address: UInt, data: UInt) = {
    val payload = BusMasterWritePayload()
    payload.address := address
    payload.data := data
    payload
  }
}

case class TilelinkBusCycleProxy(slaveParams: tilelink.BusParameter, mastersParams: tilelink.BusParameter) extends BusCycleProxy(
  new bus.tilelink.Bus(slaveParams),
  new bus.tilelink.Bus(mastersParams),
  new bus.tilelink.SlaveFactory(_, true),
  new TilelinkMasterHelper(_)
)

case class TilelinkMasterHelper(bus: tilelink.Bus) extends Area with BusMasterSend {
  override def send(stream: Stream[BusMasterWritePayload]) = new Area {
    val out = stream.map(payload => {
      val channelA = tilelink.ChannelA(bus.a.p)
      channelA.opcode   := tilelink.Opcode.A.PUT_FULL_DATA
      channelA.size     := 2
      channelA.source   := 0
      channelA.address  := payload.address.resized
      channelA.data     := payload.data.asBits.resized
      channelA.debugId  := 0
      channelA.mask     := 0xf
      channelA
    })

    bus.a <-< out
    bus.d.ready := True
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
