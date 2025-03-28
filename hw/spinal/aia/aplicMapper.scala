package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, AllMapping, SingleMapping}

case class aplicMapping(
  domaincfgOffset 	: Int,
  sourcecfgOffset 	: Int,
  mmsiaddrcfgOffset 	: Int,
  mmsiaddrcfghOffset 	: Int,
  smsiaddrcfgOffset 	: Int,
  smsiaddrcfghOffset 	: Int,
  setipOffset 	: Int,
  setipnumOffset 	: Int,
  in_clripOffset 	: Int,
  clripnumOffset 	: Int,
  setieOffset 	: Int,
  setienumOffset 	: Int,
  clrieOffset 	: Int,
  clrienumOffset 	: Int,
  setipnum_leOffset 	: Int,
  setipnum_beOffset 	: Int,
  genmsiOffset 	: Int,
  targetOffset 	: Int,
  idcOffset 		: Int,
  idShift : Int,
  idcGroup : Int,
  ideliveryOffset : Int,
  iforceOffset : Int,
  ithresholdOffset : Int,
  topiOffset : Int,
  claimiOffset : Int
)

object aplicMapping{
  def aplicMap = aplicMapping(
  domaincfgOffset 	= 0x0000,
  sourcecfgOffset 	= 0x0004 - 4,
  mmsiaddrcfgOffset 	= 0x1BC0,
  mmsiaddrcfghOffset 	= 0x1BC4,
  smsiaddrcfgOffset 	= 0x1BC8,
  smsiaddrcfghOffset 	= 0x1BCC,
  setipOffset 	= 0x1C00,
  setipnumOffset 	= 0x1CDC,
  in_clripOffset 	= 0x1D00,
  clripnumOffset 	= 0x1DDC,
  setieOffset 	= 0x1E00,
  setienumOffset 	= 0x1EDC,
  clrieOffset 	= 0x1F00,
  clrienumOffset 	= 0x1FDC,
  setipnum_leOffset 	= 0x2000,
  setipnum_beOffset 	= 0x2004,
  genmsiOffset 	= 0x3000,
  targetOffset 	= 0x3004 - 4,
  idcOffset 		= 0x4000,
  idShift = 2,
  idcGroup = 32,
  ideliveryOffset = 0x00,
  iforceOffset = 0x04,
  ithresholdOffset = 0x08,
  topiOffset = 0x18,
  claimiOffset = 0x1c

  )
}

object aplicMapper{
	def apply(bus: BusSlaveFactory, mapping: aplicMapping)(domaincfg : domaincfg, setStatecfg : setState, sources : Seq[source], idcs : Seq[idc], interrupts : Seq[APLICInterruptSource]) = new Area{
    import mapping._

    bus.read(domaincfg.align, address = domaincfgOffset, bitOffset = 24)
    bus.readAndWrite(domaincfg.ie, address = domaincfgOffset, bitOffset = 8)
    bus.readAndWrite(domaincfg.dm, address = domaincfgOffset, bitOffset = 2)
    bus.readAndWrite(domaincfg.be, address = domaincfgOffset, bitOffset = 0)

    bus.read(setStatecfg.setipnum, address = setipnumOffset)
    val setipnum = bus.createAndDriveFlow(UInt(32 bits), setipnumOffset)
    when(setipnum.valid){
      setStatecfg.setipnum := setipnum.payload.asBits
      setip(interrupts, setipnum.payload, True)
    }

    bus.read(setStatecfg.clripnum, address = clripnumOffset)
    val clripnum = bus.createAndDriveFlow(UInt(32 bits), clripnumOffset)
    when(clripnum.valid){
      setStatecfg.clripnum := clripnum.payload.asBits
      setip(interrupts, clripnum.payload, False)
    }

    bus.readAndWrite(setStatecfg.setienum, address = setienumOffset)
    bus.onWrite(address = setienumOffset){
      sources(setStatecfg.setienum.asUInt.resized).ie := True
    }
    bus.readAndWrite(setStatecfg.clrienum, address = clrienumOffset)
    bus.onWrite(address = clrienumOffset){
      sources(setStatecfg.clrienum.asUInt.resized).ie := False
    }

    bus.read(B(0), address = setipOffset, bitOffset = 0)
    bus.read(B(0), address = setieOffset, bitOffset = 0)
    val sourceMapping = for(source <- sources) yield new Area{
      bus.readAndWrite(source.mode, address = sourcecfgOffset + (source.id << idShift), bitOffset = 0)
      bus.readAndWrite(source.D, address = sourcecfgOffset + (source.id << idShift), bitOffset = 10)

      bus.readAndWrite(source.ie, address = setieOffset + (source.id/bus.busDataWidth)*bus.busDataWidth/8,
                       bitOffset = source.id % bus.busDataWidth)
      bus.readAndWrite(source.iprio, address = targetOffset + (source.id << idShift), bitOffset = 0)
      bus.readAndWrite(source.hartindex, address = targetOffset + (source.id << idShift), bitOffset = 18)
    }

    val interuptMapping = for(interrupt <- interrupts) yield new Area{
      bus.readAndWrite(interrupt.ip, address = setipOffset + (interrupt.id/bus.busDataWidth)*bus.busDataWidth/8,
                       bitOffset = interrupt.id % bus.busDataWidth)
    }

    val idWidth = log2Up((sources.map(_.id) ++ Seq(0)).max + 1)
    val claim = Flow(UInt(idWidth bits))
    claim.valid := False
    claim.payload.assignDontCare()
    when(claim.valid) {
      AIAOperator.doClaim(claim.payload, interrupts)
    }

    // val coherencyStall = Counter(2)
    // when(coherencyStall =/= 0){
    //   bus.readHalt()
    //   coherencyStall.increment()
    // }
    // bus.onReadPrimitive(AllMapping, haltSensitive = false, documentation = ""){
    //   coherencyStall.increment()
    // }
    // bus.onWritePrimitive(AllMapping, haltSensitive = false, documentation = ""){
    //   coherencyStall.increment()
    // }

    val targetMapping = for(idc <- idcs) yield new Area {
      val idcThisOffset = idcOffset + (idc.id * idcGroup)
      val nowRequest = idc.generic.bestRequest.asInstanceOf[APLICRequest]

      bus.readAndWrite(idc.idelivery, address = idcThisOffset + ideliveryOffset)
      bus.readAndWrite(idc.iforce, address = idcThisOffset + iforceOffset)
      bus.readAndWrite(idc.ithreshold, address = idcThisOffset + ithresholdOffset)
      // topi readonly
      bus.read(nowRequest.prio, address = idcThisOffset + topiOffset, bitOffset = 0)
      bus.read(nowRequest.id, address = idcThisOffset + topiOffset, bitOffset = 16)
      // reading claimi trigger clean the top interrupt
      bus.read(nowRequest.prio, address = idcThisOffset + claimiOffset, bitOffset = 0)
      bus.read(nowRequest.id, address = idcThisOffset + claimiOffset, bitOffset = 16)
      bus.onRead(address = idcThisOffset + claimiOffset) {
        claim.valid := True
        claim.payload := nowRequest.id
      }
    }

	}

  def setip(interrupts : Seq[APLICInterruptSource], id : UInt, state : Bool){
      for (interrupt <- interrupts) {
        when (interrupt.id === id) {
          interrupt.ip := state
        }
      }
  }
}


