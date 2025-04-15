package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink._
import spinal.core.fiber.Fiber

class TilelinkMaster(m2sParams: M2sParameters, p : BusParameter) extends Component {
  val io = new Bundle {
    val busMaster = master(Bus(m2sParams))
    val busSlave = slave(Bus(p))
  }

  val busA = io.busMaster.a
  val busD = io.busMaster.d
  busD.ready := True
  busA.valid := False
  busA.payload.assignDontCare()

  val address = RegInit(Bits(32 bits))

  val factoryGen = new bus.tilelink.SlaveFactory(_, true)
  val factory = factoryGen(io.busSlave)

  val data = factory.createAndDriveFlow(UInt(32 bits), address = 0x0)
  when(data.valid){
    busA.valid    := True
    busA.opcode   := Opcode.A.PUT_FULL_DATA
    busA.size     := 4
    busA.source   := 0
    busA.address  := 0x10000000
    busA.data     := data.payload.asBits
    busA.debugId  := 0
    busA.mask     := 0xf
  }
}

case class TilelinkMasterFiber() extends Area {
  val nodeM = fabric.Node.master()
  val nodeS = fabric.Node.slave()

  val m2sParams = M2sParameters(
    addressWidth = 32,
    dataWidth = 32,
    masters = List(
      M2sAgent(
        name = TilelinkMasterFiber.this,
        mapping = List(
          M2sSource(
            id = SizeMapping(0, 4),
            emits = M2sTransfers(
              get = SizeRange(1, 64),
              putFull = SizeRange(1, 64)
            )
          )
        )
      )
    )
  )

  val fiber = Fiber build new Area {
    nodeM.m2s forceParameters m2sParams
    nodeM.s2m.supported load S2mSupport.none()

    // nodeS.m2s.supported.load(nodeS.m2s.proposed)
    nodeS.m2s.supported.load(TilelinkAplic.getTilelinkSupport(nodeS.m2s.proposed))
    nodeS.s2m.none()

    val masterNode = new TilelinkMaster(m2sParams, nodeS.bus.p)

    // nodeM.bus.a.setIdle()
    // nodeM.bus.d.ready := True

    masterNode.io.busMaster <> nodeM.bus
    masterNode.io.busSlave <> nodeS.bus
  }
}
