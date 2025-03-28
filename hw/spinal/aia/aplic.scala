package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

class MappedAplic[T <: spinal.core.Data with IMasterSlave](sourceIds : Seq[Int],
                                                           hartIds : Seq[Int],
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component{
  val aplicMap = APlicMapping.aplicMap

  val io = new Bundle {
    val bus = slave(busType())
    val sources = in Bits (sourceIds.size bits)
    val targets = out Bits (hartIds.size bits)
  }

  val domaincfg = new domaincfg()
  val setState = new setState()

  // sourceids
  val sources = for (sourceId <- sourceIds) yield new APlicSource(sourceId)

  val interrupts = for (source <- sources)
    yield new APLICInterruptSource(source.id, source.hartindex.getWidth, source.iprio.getWidth){
      ie := source.ie
      target := source.hartindex
      prio := source.iprio
    }

  val gateways = for ((source, idx) <- sources.zipWithIndex) yield
                 new APlicGateway(io.sources(idx), idx, source, domaincfg, interrupts(idx))

  // hartids
  val idcs = for (hartId <- hartIds) yield new APlicIDC(interrupts, hartId)

  io.targets := idcs.map(_.output).asBits

  val factory = factoryGen(io.bus)
  val mapping = APlicMapper(factory, aplicMap)(
    domaincfg = domaincfg,
    setStatecfg = setState,
    sources = sources,
    idcs = idcs,
    interrupts = interrupts
  )

  /*TODO:
   * 1. source interupt re-triiger
   * 2x. setipReg reset to 0?
   * 3. gateway -> ie
   * 4. allowUnsetRegToAvoidLatch()
   * 5. replace magic number with enum APlicSourceMode
   */
}

case class TilelinkAplic(sourceIds : Seq[Int], hartIds : Seq[Int], p : bus.tilelink.BusParameter) extends MappedAplic[bus.tilelink.Bus](
  sourceIds,
  hartIds,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)

object APlicSourceMode extends SpinalEnum {
  val inactive, detached, rising, falling, high, low = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    inactive -> 0,
    detached -> 1,
    rising -> 4,
    falling -> 5,
    high -> 6,
    low -> 7)
}

case class domaincfg() extends Area {
  val align = RegInit(U(0x80, 8 bits))
  val ie = RegInit(False)
  val dm = RegInit(False)
  val be = RegInit(False)
}

// sourceIds
case class APlicSource(id : Int) extends Bundle {
  val D = RegInit(False)
  val mode = RegInit(B(0x0, 10 bits))
  val ie = RegInit(False)

  val hartindex = RegInit(U(0x0, 14 bits))
  // for direct delivery mode
  val iprio = RegInit(U(0x0, 8 bits))
  // for msi delivery mode
  val guestindex = RegInit(U(0x0, 6 bits))
  val eiid = RegInit(U(0x0, 11 bits))

  val triiger = APlicSourceMode()
  when(D === False){
    switch (mode){
      is(0, 1, 4, 5, 6, 7){
        triiger.assignFromBits(mode.resized)
      }
      default{
        triiger := APlicSourceMode.inactive
      }
    }
  }otherwise{
    triiger := APlicSourceMode.inactive
  }
}

case class setState() extends Area {
  val setipnum = RegInit(U(0x0, 32 bits))
  val clripnum = RegInit(U(0x0, 32 bits))
  val setienum = RegInit(U(0x0, 32 bits))
  val clrienum = RegInit(U(0x0, 32 bits))
}

// hartIds
case class APlicIDC(interrupts : Seq[APLICInterruptSource], id : Int) extends Bundle{
  val idelivery = RegInit(False)
  val iforce = RegInit(False)
  val ithreshold = RegInit(U(0x0, 8 bits))

  // topi can be found in generic.bestRequest
  val generic = AIAGeneric(interrupts, id)
  generic.threshold := ithreshold.resized

  val output = generic.claim > 0
}

case class APlicGateway(input : Bool, idx : UInt, source : APlicSource, domaincfg : domaincfg, interrupt : AIAInterruptSource) extends Area{
  when(domaincfg.ie === True){
    when(source.D === True){
      source.ie := False
    }otherwise {
      switch(source.triiger){
        is(APlicSourceMode.inactive){
          interrupt.ip := False
          source.ie := False
        }
        is(APlicSourceMode.detached){
          source.ie := True
        }
        is(APlicSourceMode.rising){
          when(input.rise()){
            source.ie := True
            interrupt.ip := True
          }
        }
        is(APlicSourceMode.falling){
          when(input.fall()){
            source.ie := True
            interrupt.ip := True
          }
        }
        is(APlicSourceMode.high){
          when(input === True){
            source.ie := True
            interrupt.ip := True
          }
        }
        is(APlicSourceMode.low){
          when(input === False){
            source.ie := True
            interrupt.ip := True
          }
        }
      }
    }
  }
}

case class APLICRequest(idWidth : Int, priorityWidth: Int) extends AIARequest(idWidth) {
  val prio = UInt(priorityWidth bits)

  override def prioritize(other: AIARequest): Bool = {
    val x = other.asInstanceOf[APLICRequest]
    !x.valid || (valid && ((prio < x.prio) || ((prio === x.prio) && (id <= x.id))))
  }

  override def pending(threshold: UInt): Bool = {
    valid && ((threshold === 0) || (prio < threshold))
  }

  override def dummy(): AIARequest = {
    val tmp = APLICRequest(idWidth, priorityWidth)
    tmp.id := 0
    tmp.valid := False
    tmp.prio := 0
    tmp
  }
}

case class APLICInterruptSource(sourceId : Int, idWidth : Int, priorityWidth : Int) extends AIAInterruptSource(sourceId) {
  val target = UInt(idWidth bits)
  val prio = UInt(priorityWidth bits)

  override def asRequest(idWidth : Int, targetHart : Int): AIARequest = {
    val ret = new APLICRequest(idWidth, priorityWidth)
    ret.id := U(id)
    ret.valid := ip && ie && (target === targetHart)
    ret.prio := prio
    ret
  }
}
