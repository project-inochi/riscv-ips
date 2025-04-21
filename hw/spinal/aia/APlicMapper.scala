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

case class APlicMSIPayload() extends Bundle {
  val address = UInt(64 bits)
  val data = UInt(32 bits)
}

trait APlicBusMasterSend {
  def send(stream: Stream[APlicMSIPayload]): Area
}

object APlicMapper {
  def apply(slaveBus: BusSlaveFactory, masterBus: APlicBusMasterSend, mapping: APlicMapping)(aplic: APlic) = new Area{
    import mapping._

    val domaincfg = new Area {
      slaveBus.read(U(0x80, 8 bits), address = domaincfgOffset, bitOffset = 24)
      slaveBus.readAndWrite(aplic.domainEnable, address = domaincfgOffset, bitOffset = 8)
      slaveBus.readAndWrite(aplic.isMSI, address = domaincfgOffset, bitOffset = 2)
      slaveBus.readAndWrite(aplic.bigEndian, address = domaincfgOffset, bitOffset = 0)
    }

    // mapping GENMSI, MSIADDRCFG, MSIADDRCFGH
    val msi = new Area {
      val msiaddrReg = aplic.msiaddrcfg

      slaveBus.read(Mux(msiaddrReg.readable, msiaddrReg.ppn(31 downto 0), U(0)), address = mmsiaddrcfgOffset)
      slaveBus.read(Mux(msiaddrReg.readable, msiaddrReg.msiaddrh, U(0)), address = mmsiaddrcfghOffset)

      val msiaddrcfg = slaveBus.createAndDriveFlow(UInt(32 bits), mmsiaddrcfgOffset)
      when(msiaddrcfg.valid && !msiaddrReg.lock && aplic.isMSI) {
        msiaddrReg.ppn(31 downto 0) := msiaddrcfg.payload
      }

      /* when receiving message that make lock 1 to 0, should the other
      * bits be immediately written into Reg?
      */
      val msiaddrhcfg = slaveBus.createAndDriveFlow(UInt(32 bits), mmsiaddrcfghOffset)
      when(msiaddrhcfg.valid && aplic.isMSI) {
        msiaddrReg.lock := msiaddrhcfg.payload(31)
        when(!msiaddrReg.lock) {
          msiaddrReg.hhxs := msiaddrhcfg.payload(28 downto 24)
          msiaddrReg.lhxs := msiaddrhcfg.payload(22 downto 20)
          msiaddrReg.hhxw := msiaddrhcfg.payload(18 downto 16)
          msiaddrReg.lhxw := msiaddrhcfg.payload(15 downto 12)
          msiaddrReg.ppn(43 downto 32) := msiaddrhcfg.payload(11 downto 0)
        }
      }

      val genmsiFlow = slaveBus.createAndDriveFlow(UInt(32 bits), genmsiOffset)
      val (genmsiFlowStream, _) = genmsiFlow.queueWithOccupancy(2)
      val genmsiStream = genmsiFlowStream.map(params => {
        val payload = APlicMSIPayload()
        payload.address := msiaddrReg.msiAddress(params(31 downto 18)).resized
        payload.data := params(10 downto 0).resized
        payload
      })

      val msiStream = StreamArbiterFactory().lowerFirst.noLock.onArgs(aplic.msiStream, genmsiStream)

      masterBus.send(msiStream)
    }

    // mapping SETIPNUM, CLRIPNUM, SETIENUM, CLRIPNUM
    val numOPs = new Area {
      def mapNumArea(offset: Int, func: UInt => Unit, doc: String = null) = new Area {
        val numFlow = slaveBus.createAndDriveFlow(UInt(32 bits), address = offset, documentation = doc)
        when(numFlow.valid) {
          func(numFlow.payload)
        }
      }

      val setipnum = mapNumArea(setipnumOffset, APlic.doSet(aplic.interrupts, _))
      val clripnum = mapNumArea(clripnumOffset, APlic.doClaim(aplic.interrupts, _))
      val setienum = mapNumArea(setienumOffset, APlic.doEnable(aplic.interrupts, _))
      val clrienum = mapNumArea(clrienumOffset, APlic.doDisable(aplic.interrupts, _))
    }

    slaveBus.read(B(0), address = setipOffset, bitOffset = 0)
    slaveBus.read(B(0), address = setieOffset, bitOffset = 0)

    // mapping SOURCECFG, TARGET, SETIE, SETIP for interrupt
    val interruptMapping = for(interrupt <- aplic.interrupts) yield new Area {
      val interruptOffset = (interrupt.id / slaveBus.busDataWidth) * slaveBus.busDataWidth / 8
      val interruptBitOffset = interrupt.id % slaveBus.busDataWidth
      val configOffset = (interrupt.id - 1) * 4

      val configFlow = slaveBus.createAndDriveFlow(UInt(11 bits), sourcecfgOffset + configOffset)
      when(configFlow.valid) {
        interrupt.setConfig(configFlow.payload)
      }

      slaveBus.readAndWrite(interrupt.ie, address = setieOffset + interruptOffset, bitOffset = interruptBitOffset)

      slaveBus.read(interrupt.ip, address = setipOffset + interruptOffset, bitOffset = interruptBitOffset)
      val ipDrive = slaveBus.createAndDriveFlow(Bool(), address = setipOffset + interruptOffset, bitOffset = interruptBitOffset)
      when(ipDrive.valid) {
        interrupt.doPendingUpdate(ipDrive.payload)
      }

      slaveBus.readAndWrite(interrupt.prio, address = targetOffset + configOffset, bitOffset = 0)
      slaveBus.readAndWrite(interrupt.targetId, address = targetOffset + configOffset, bitOffset = 18)
    }

    // mapping interrupt delivery control for each gateway
    val idcs = for(idc <- aplic.directGateways) yield new Area {
      val idcThisOffset = idcOffset + (idc.hartId * idcGroup)
      val nowRequest = idc.bestRequest.asInstanceOf[APlicDirectRequest]

      slaveBus.readAndWrite(idc.idelivery, address = idcThisOffset + ideliveryOffset)
      slaveBus.readAndWrite(idc.iforce, address = idcThisOffset + iforceOffset)
      slaveBus.readAndWrite(idc.ithreshold, address = idcThisOffset + ithresholdOffset)
      // topi readonly
      slaveBus.read(nowRequest.prio, address = idcThisOffset + topiOffset, bitOffset = 0)
      slaveBus.read(nowRequest.id, address = idcThisOffset + topiOffset, bitOffset = 16)
      // reading claimi trigger clean the top interrupt
      slaveBus.read(nowRequest.prio, address = idcThisOffset + claimiOffset, bitOffset = 0)
      slaveBus.read(nowRequest.id, address = idcThisOffset + claimiOffset, bitOffset = 16)
      slaveBus.onRead(address = idcThisOffset + claimiOffset) {
        idc.doBestClaim()
      }
    }
  }
}
