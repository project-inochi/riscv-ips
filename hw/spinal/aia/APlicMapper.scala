package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, AllMapping, SingleMapping}

case class APlicMapping(
  domaincfgOffset     : Int,
  sourcecfgOffset     : Int,
  mmsiaddrcfgOffset   : Int,
  mmsiaddrcfghOffset  : Int,
  smsiaddrcfgOffset   : Int,
  smsiaddrcfghOffset  : Int,
  setipOffset         : Int,
  setipnumOffset      : Int,
  in_clripOffset      : Int,
  clripnumOffset      : Int,
  setieOffset         : Int,
  setienumOffset      : Int,
  clrieOffset         : Int,
  clrienumOffset      : Int,
  setipnum_leOffset   : Int,
  setipnum_beOffset   : Int,
  genmsiOffset        : Int,
  targetOffset        : Int,
  idcOffset           : Int,
  idcGroup            : Int,
  ideliveryOffset     : Int,
  iforceOffset        : Int,
  ithresholdOffset    : Int,
  topiOffset          : Int,
  claimiOffset        : Int
)

object APlicMapping {
  def aplicMap = APlicMapping(
    domaincfgOffset     = 0x0000,
    sourcecfgOffset     = 0x0004,
    mmsiaddrcfgOffset   = 0x1BC0,
    mmsiaddrcfghOffset  = 0x1BC4,
    smsiaddrcfgOffset   = 0x1BC8,
    smsiaddrcfghOffset  = 0x1BCC,
    setipOffset         = 0x1C00,
    setipnumOffset      = 0x1CDC,
    in_clripOffset      = 0x1D00,
    clripnumOffset      = 0x1DDC,
    setieOffset         = 0x1E00,
    setienumOffset      = 0x1EDC,
    clrieOffset         = 0x1F00,
    clrienumOffset      = 0x1FDC,
    setipnum_leOffset   = 0x2000,
    setipnum_beOffset   = 0x2004,
    genmsiOffset        = 0x3000,
    targetOffset        = 0x3004,

    idcOffset           = 0x4000,
    idcGroup            = 32,
    ideliveryOffset     = 0x00,
    iforceOffset        = 0x04,
    ithresholdOffset    = 0x08,
    topiOffset          = 0x18,
    claimiOffset        = 0x1c
  )
}

object APlicMapper {
  def apply(bus: BusSlaveFactory, mapping: APlicMapping)(aplic: APlic) = new Area{
    import mapping._

    bus.read(U(0x80, 8 bits), address = domaincfgOffset, bitOffset = 24)
    bus.readAndWrite(aplic.domainEnable, address = domaincfgOffset, bitOffset = 8)
    bus.readAndWrite(aplic.deliveryMode, address = domaincfgOffset, bitOffset = 2)
    bus.readAndWrite(aplic.bigEndian, address = domaincfgOffset, bitOffset = 0)

    val setipnum = bus.createAndDriveFlow(UInt(32 bits), setipnumOffset)
    when(setipnum.valid){
      AIAOperator.doSet(aplic.interrupts, setipnum.payload)
    }

    val clripnum = bus.createAndDriveFlow(UInt(32 bits), clripnumOffset)
    when(clripnum.valid){
      AIAOperator.doClaim(aplic.interrupts, clripnum.payload)
    }

    val setienum = bus.createAndDriveFlow(UInt(32 bits), setienumOffset)
    when(setienum.valid){
      AIAOperator.enable(aplic.interrupts, setienum.payload)
    }

    val clrienum = bus.createAndDriveFlow(UInt(32 bits), clrienumOffset)
    when(clrienum.valid){
      AIAOperator.disable(aplic.interrupts, clrienum.payload)
    }

    bus.read(B(0), address = setipOffset, bitOffset = 0)
    bus.read(B(0), address = setieOffset, bitOffset = 0)
    val interruptMapping = for(interrupt <- aplic.interrupts) yield new Area{
      val sourceflow = bus.createAndDriveFlow(UInt(11 bits), sourcecfgOffset + ((interrupt.id - 1) * 4))
      when(sourceflow.valid) {
        interrupt.setConfig(sourceflow.payload)
      }

      bus.readAndWrite(interrupt.prio, address = targetOffset + ((interrupt.id - 1) * 4), bitOffset = 0)
      bus.readAndWrite(interrupt.target, address = targetOffset + ((interrupt.id - 1) * 4), bitOffset = 18)
    }

    val interuptMapping = for(interrupt <- aplic.interrupts) yield new Area {
      val interruptOffset = (interrupt.id / bus.busDataWidth) * bus.busDataWidth / 8
      val interruptBitOffset = interrupt.id % bus.busDataWidth

      bus.readAndWrite(interrupt.ie, address = setieOffset + interruptOffset,
                       bitOffset = interruptBitOffset)

      bus.read(interrupt.ip, address = setipOffset + interruptOffset, bitOffset = interruptBitOffset)
      val ipDrive = bus.createAndDriveFlow(Bool(), address = setipOffset + interruptOffset, bitOffset = interruptBitOffset)

      when(ipDrive.valid) {
        interrupt.doPendingUpdate(ipDrive.payload)
      }
    }

    val idWidth = log2Up((aplic.interrupts.map(_.id) ++ Seq(0)).max + 1)
    val claim = Flow(UInt(idWidth bits))
    claim.valid := False
    claim.payload.assignDontCare()
    when(claim.valid) {
      AIAOperator.doClaim(aplic.interrupts, claim.payload)
    }

    val targetMapping = for(idc <- aplic.directGateways) yield new Area {
      val idcThisOffset = idcOffset + (idc.id * idcGroup)
      val nowRequest = idc.generic.bestRequest.asInstanceOf[APlicRequest]

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
}
