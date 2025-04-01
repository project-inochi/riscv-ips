package aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

class MappedAplic[T <: spinal.core.Data with IMasterSlave](sourceIds : Seq[Int],
                                                           hartIds : Seq[Int],
                                                           slaves : Seq[MappedAplic[T]],
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component{
  val aplicMap = APlicMapping.aplicMap

  val io = new Bundle {
    val bus = slave(busType())
    val sources = in Bits (sourceIds.size bits)
    val targets = out Bits (hartIds.size bits)
    val slaveio = slaves.nonEmpty generate Vec(slaves.map(slave => out Bits (slave.interrupts.length bits)))
    //val slaveio = out Bits (sourceIds.size bits)
  }

  // io.slaveio.foreach(slaves(0).io.sources := _(0))

  val domaincfg = new domaincfg()

  val interrupts = for (i <- 1 to sourceIds.max) yield new APLICInterruptSource(i)

  val gateways = for ((interrupt, idx) <- interrupts.zipWithIndex) yield
                 new APlicGateway(io.sources(idx), idx, domaincfg, interrupt, io.slaveio)

  // hartids
  val idcs = for (i <- 0 to hartIds.max) yield new APlicIDC(interrupts, i)

  io.targets := idcs.map(_.output).asBits

  val factory = factoryGen(io.bus)
  val mapping = APlicMapper(factory, aplicMap)(
    domaincfg = domaincfg,
    idcs = idcs,
    interrupts = interrupts
  )

  /*TODO:
   * complete sim process
   * MSI
   * child
   */
}

case class TilelinkAplic(sourceIds : Seq[Int], hartIds : Seq[Int], slaves : Seq[TilelinkAplic], p : bus.tilelink.BusParameter) extends MappedAplic[bus.tilelink.Bus](
  sourceIds,
  hartIds,
  slaves,
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

case class APlicGateway(input : Bool, idx : UInt, domaincfg : domaincfg, interrupt : APLICInterruptSource, slaveio : Vec[Bits]) extends Area{
  when(domaincfg.ie === True){
    when(interrupt.D === True){
      interrupt.ie := False
      slaveio(0)(idx) := input
    }otherwise {
      // should high resistance
      slaveio(0)(idx) := False
      switch(interrupt.triiger){
        is(APlicSourceMode.inactive){
          interrupt.ip := False
          interrupt.ie := False
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

case class APLICInterruptSource(sourceId : Int) extends AIAInterruptSource(sourceId) {
  val D = RegInit(False)
  val mode = RegInit(B(0x0, 10 bits))

  val target = RegInit(U(0x0, 14 bits))
  val prio = RegInit(U(0x0, 8 bits))
  val triigerLevel = Bool()

  // for msi delivery mode
  val guestindex = RegInit(U(0x0, 6 bits))
  val eiid = RegInit(U(0x0, 11 bits))

  val triiger = APlicSourceMode()
  triigerLevel := (triiger === APlicSourceMode.high) || (triiger === APlicSourceMode.low)

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

  override def asRequest(idWidth : Int, targetHart : Int): AIARequest = {
    val ret = new APLICRequest(idWidth, prio.getWidth)
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
