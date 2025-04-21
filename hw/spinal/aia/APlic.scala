package aia

import spinal.core._
import spinal.lib._

case class APlicGenParam(withIEP: Boolean,
                         withMSI: Boolean,
                         msiFifoSize: Int = 8)

object APlicGenParam {
  def full = APlicGenParam(
    withIEP     = true,
    withMSI     = true,
  )

  def MSI = APlicGenParam(
    withIEP     = false,
    withMSI     = true,
  )

  def light = APlicGenParam(
    withIEP     = false,
    withMSI     = false,
  )
}

case class APlicDomainParam(isRoot: Boolean,
                            isMDomain: Boolean,
                            genParam: APlicGenParam)

object APlicDomainParam {
  def root(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = true,
    isMDomain = true,
    genParam  = genParam,
  )

  def M(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = false,
    isMDomain = true,
    genParam  = genParam,
  )

  def S(genParam: APlicGenParam) = APlicDomainParam(
    isRoot    = false,
    isMDomain = false,
    genParam  = genParam,
  )
}

case class APlicSlaveInfo(childIdx: Int, sourceIds: Seq[Int])

case class APlic(sourceIds: Seq[Int], hartIds: Seq[Int], slaveInfos: Seq[APlicSlaveInfo]) extends Area {
  require(sourceIds.distinct.size == sourceIds.size, "APlic requires no duplicate interrupt source")
  require(hartIds.distinct.size == hartIds.size, "APlic requires no duplicate harts")

  val sources = Bits(sourceIds.size bits)
  val directTargets = Bits(hartIds.size bits)
  val slaveSources = Vec(slaveInfos.map(slaveInfo => Bits(slaveInfo.sourceIds.size bits)))

  val slaveInterruptIds = slaveInfos.flatMap(slaveInfo => slaveInfo.sourceIds).distinct
  val interruptDelegatable = for (sourceId <- sourceIds) yield slaveInterruptIds.find(_ == sourceId).isDefined

  val domainEnable = RegInit(False)
  val isMSI = RegInit(False)
  val bigEndian = False

  val msiaddrcfg = new Area {
    val lock = RegInit(False)
    val ppn = RegInit(U(0x0, 44 bits))
    val hhxs = RegInit(U(0x0, 5 bits))
    val lhxs = RegInit(U(0x0, 3 bits))
    val hhxw = RegInit(U(0x0, 3 bits))
    val lhxw = RegInit(U(0x0, 4 bits))

    val maskH = U(1) << hhxw - 1
    val maskL = U(1) << lhxw - 1

    val readable = !lock && isMSI

    val msiaddrh = U(0x0, 32 bits)
    msiaddrh(31) := lock
    msiaddrh(28 downto 24) := hhxs
    msiaddrh(22 downto 20) := lhxs
    msiaddrh(18 downto 16) := hhxw
    msiaddrh(15 downto 12) := lhxw
    msiaddrh(11 downto 0) := ppn(43 downto 32)

    def msiAddress(hartIndex: UInt, guestIndex: UInt = 0): UInt = {
      val groupId = (hartIndex >> lhxw) & maskH.resized
      val hartId = hartIndex & maskL.resized
      val groupOffset = groupId << (hhxs + 12)
      val hartOffset = hartId << lhxs

      val msiaddr = (ppn | groupOffset.resized | hartOffset.resized | guestIndex.resized) << 12
      msiaddr
    }
  }

  val interrupts: Seq[APlicSource] = for (((sourceId, delegatable), i) <- sourceIds.zip(interruptDelegatable).zipWithIndex)
    yield new APlicSource(sourceId, delegatable, APlicDomainState(domainEnable, isMSI), sources(i))

  val slaveMappings = for ((slaveInfo, slaveSource) <- slaveInfos.zip(slaveSources)) yield new Area {
    for ((slaveSourceId, idx) <- slaveInfo.sourceIds.zipWithIndex) yield new Area {
      interrupts.find(_.id == slaveSourceId).map(interrupt => new Area {
        when(domainEnable && interrupt.delegated && (Bool(slaveInfos.size == 1) || interrupt.childIdx === slaveInfo.childIdx)) {
          slaveSource(idx) := interrupt.input
        } otherwise {
          slaveSource(idx) := False
        }
      })
    }
  }

  // hartids
  val directGateways = for (hartId <- hartIds) yield new APlicDirectGateway(interrupts, hartId, !isMSI)

  val msiGateway = new APlicMSIGateway(interrupts, isMSI)

  val msiStream = msiGateway.requestStream.map(req => {
    val payload = APlicMSIPayload()
    payload.address := msiaddrcfg.msiAddress(req.target.hartId, req.target.guestId).resized
    payload.data := req.target.eiid.resized
    payload
  })

  directTargets := Mux(isMSI, B(0), directGateways.map(_.iep).asBits())
}

object APlic {
  def doWhenMatch(interrupts: Seq[APlicSource], id: UInt, func: APlicSource => Unit) = new Area {
    switch(id) {
      for (interrupt <- interrupts) {
        is (interrupt.id) {
          func(interrupt)
        }
      }
    }
  }

  def doClaim(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doClaim())

  def doSet(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doSet())

  def doEnable(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doEnable())

  def doDisable(interrupts: Seq[APlicSource], id: UInt) = doWhenMatch(interrupts, id, _.doDisable())
}
