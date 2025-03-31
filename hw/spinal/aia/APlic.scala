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
      triigerLevel := (source.triiger === APlicSourceMode.high) || (source.triiger === APlicSourceMode.low)
    }

  val gateways = for ((source, idx) <- sources.zipWithIndex) yield
                 new APlicGateway(io.sources(idx), idx, source, domaincfg, interrupts(idx))

  // hartids
  val idcs = for (i <- 0 to hartIds.max) yield new APlicIDC(interrupts, i)

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
   * 1x. use createAndDriveFlow to achieve setienum/clrienum
   * 2. complete sim process
   * 3. MSI
   * 4x. rename filename
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
  val ie = RegInit(False)
  val dm = RegInit(False)
  val be = RegInit(False)
}

// sourceIds
case class APlicSource(id : Int) extends Area {
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
    switch(mode) {
      for (state <- APlicSourceMode.elements) {
        is(state.asBits.resized) {
          triiger := state
        }
      }
      default {
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

        }
        is(APlicSourceMode.rising){
          when(input.rise()){
            interrupt.ip := True
          }
        }
        is(APlicSourceMode.falling){
          when(input.fall()){
            interrupt.ip := True
          }
        }
        is(APlicSourceMode.high){
            interrupt.ip := input
        }
        is(APlicSourceMode.low){
            interrupt.ip := ~input
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
  val triigerLevel = Bool()

  override def asRequest(idWidth : Int, targetHart : Int): AIARequest = {
    val ret = new APLICRequest(idWidth, priorityWidth)
    ret.id := U(id)
    ret.valid := ip && ie && (target === targetHart)
    ret.prio := prio
    ret
  }

  override def doClaim(): Unit = {
    when(triigerLevel === False){
      ip := False
    }
  }

  override def doSet(): Unit = {
    when(triigerLevel === False){
      ip := True
    }
  }
}
