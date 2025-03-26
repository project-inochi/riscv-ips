package aplic

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
  sourcecfgOffset 	= 0x0004,
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
  targetOffset 	= 0x3004,
  idcOffset 		= 0x4000,
  idShift = 2,
  idcGroup = 20,
  ideliveryOffset = 0x00,
  iforceOffset = 0x04,
  ithresholdOffset = 0x08,
  topiOffset = 0x18,
  claimiOffset = 0x1c

  )
}

object aplicMapper{
	def apply(bus: BusSlaveFactory, mapping: aplicMapping)(domaincfg : domaincfg, setStatecfg : setState, sources : Seq[source], idcs : Seq[idc]) = new Area{
    import mapping._

    bus.read(domaincfg.align, address = domaincfgOffset, bitOffset = 24)
    bus.driveAndRead(domaincfg.ie, address = domaincfgOffset, bitOffset = 8)
    bus.driveAndRead(domaincfg.dm, address = domaincfgOffset, bitOffset = 2)
    bus.driveAndRead(domaincfg.be, address = domaincfgOffset, bitOffset = 0)

    // todo:setstatereg onread
    // not sure
    bus.driveAndRead(setStatecfg.setipnum, address = setipnumOffset)
    bus.onWrite(address = setipnumOffset){
      sources(setStatecfg.setipnum.asUInt).ip := True
    }
    bus.driveAndRead(setStatecfg.clripnum, address = clripnumOffset)
    bus.onWrite(address = clripnumOffset){
      sources(setStatecfg.clripnum.asUInt).ip := False
    }
    bus.driveAndRead(setStatecfg.setienum, address = setienumOffset)
    bus.onWrite(address = setienumOffset){
      sources(setStatecfg.setienum.asUInt).ie := True
    }
    bus.driveAndRead(setStatecfg.clrienum, address = clrienumOffset)
    bus.onWrite(address = clrienumOffset){
      sources(setStatecfg.clrienum.asUInt).ie := False
    }

    val sourceMapping = for(source <- sources) yield new Area{
      bus.driveAndRead(source.mode, address = sourcecfgOffset + (source.id << idShift), bitOffset = 0)
      bus.driveAndRead(source.D, address = sourcecfgOffset + (source.id << idShift), bitOffset = 10)

      bus.driveAndRead(source.ip, address = setipOffset + (source.id/bus.busDataWidth)*bus.busDataWidth/8,
                       bitOffset = source.id % bus.busDataWidth)
      bus.driveAndRead(source.ie, address = setieOffset + (source.id/bus.busDataWidth)*bus.busDataWidth/8,
                       bitOffset = source.id % bus.busDataWidth)

      bus.driveAndRead(source.iprio, address = targetOffset + (source.id << idShift), bitOffset = 0)
      bus.driveAndRead(source.hartindex, address = targetOffset + (source.id << idShift), bitOffset = 18)
    }

    val idWidth = log2Up((sources.map(_.id) ++ Seq(0)).max + 1)
    val claim = Flow(UInt(idWidth bits))
    claim.valid := False
    claim.payload.assignDontCare()
    when(claim.valid) {
      switch(claim.payload) {
        for (source <- sources) {
          is(source.id) {
            source.doClaim()
          }
        }
      }
    }

    val coherencyStall = Counter(2)
    when(coherencyStall =/= 0){
      bus.readHalt()
      coherencyStall.increment()
    }
    bus.onReadPrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }
    bus.onWritePrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }

    val targetMapping = for(idc <- idcs) yield new Area {
      bus.driveAndRead(idc.idelivery, address = idcOffset + (idc.id * idcGroup) + ideliveryOffset)
      bus.driveAndRead(idc.iforce, address = idcOffset + (idc.id * idcGroup) + iforceOffset)
      bus.driveAndRead(idc.ithreshold, address = idcOffset + (idc.id * idcGroup) + ithresholdOffset)
      // topi readonly
      bus.read(idc.topi_priority, address = idcOffset + (idc.id * idcGroup) + topiOffset, bitOffset = 0)
      bus.read(idc.topi_identity, address = idcOffset + (idc.id * idcGroup) + topiOffset, bitOffset = 16)
      // claimi trrigrt
      bus.read(idc.claimi_priority, address = idcOffset + (idc.id * idcGroup) + claimiOffset, bitOffset = 0)
      bus.read(idc.claimi_identity, address = idcOffset + (idc.id * idcGroup) + claimiOffset, bitOffset = 16)
      bus.onRead(address = idcOffset + (idc.id * idcGroup) + claimiOffset){
        claim.valid := True
        claim.payload := idc.claimi_identity.asUInt
      }


    }

	}
}
