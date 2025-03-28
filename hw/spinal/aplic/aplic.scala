package aplic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

class MappedAplic[T <: spinal.core.Data with IMasterSlave](sourceIds : Seq[Int],
                                                           hartIds : Seq[Int],
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component{
  val aplicMap = aplicMapping.aplicMap

  val io = new Bundle {
    val bus = slave(busType())
    val sources = in Bits (sourceIds.size bits)
    val targets = out Bits (hartIds.size bits)
    // val child0 = out Bits (sourcenum-1 bits)
  }

  // val child = Vec(io.child0)
  // val childbits = Vec(RegInit(B(0x0, sourcenum-1 bits)))
  // for(i <- 0 until sourcenum-1){
  //   child(0)(i) := childbits(0)(i)
  // }
  // childbits.allowUnsetRegToAvoidLatch()

  val domaincfg = new domaincfg()
  val setState = new setState()

  // sourceids
  val sources = for (sourceId <- sourceIds) yield new source(sourceId)

  val interrupts = for (source <- sources) yield new APLICInterruptSource(source.id, source.hartindex.getWidth,
                                                                          source.iprio.getWidth){
                                                                            ie := source.ie
                                                                            ip := False
                                                                            target := source.hartindex.asUInt
                                                                            prio := source.iprio.asUInt
                                                                          }
  val gateways = for ((source, idx) <- sources.zipWithIndex) yield
                 new aplicGateway(io.sources(idx), idx, source, domaincfg, interrupts(idx))

  // hartids
  val idcs = for (hartId <- hartIds) yield new idc(interrupts, hartId)

  io.targets := idcs.map(_.output).asBits

  val factory = factoryGen(io.bus)
  val mapping = aplicMapper(factory, aplicMap)(
    domaincfg = domaincfg,
    setStatecfg = setState,
    sources = sources,
    idcs = idcs,
    interrupts = interrupts
  )

  /*TODO:
   * 1. setstate maybe
   * x2. bus寄存器
   * x3. output分配
   * x4. threshold
   */
}

case class TilelinkAplic(sourceIds : Seq[Int], hartIds : Seq[Int], p : bus.tilelink.BusParameter) extends MappedAplic[bus.tilelink.Bus](
  sourceIds,
  hartIds,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)

object aplicSourcemode extends SpinalEnum {
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
  val align = RegInit(B(0x80, 8 bits)).allowUnsetRegToAvoidLatch()
  val ie = RegInit(False)
  val dm = RegInit(False)
  val be = RegInit(False)
}

// sourceIds
case class source(id : Int) extends Bundle {
  // val idx = RegInit(id)
  // sourcecfg
  val D = RegInit(False)
  val mode = RegInit(B(0x0, 10 bits))
  // setip
  val ie = RegInit(False)
  // val ip = RegInit(False)

  // val valid = ie && ip
  // target
  val hartindex = RegInit(B(0x0, 14 bits))
    // for direct delivery mode
  val iprio = RegInit(B(0x0, 8 bits))
    // for msi delivery mode
  val guestindex = RegInit(B(0x0, 6 bits))
  val eiid = RegInit(B(0x0, 11 bits))

  val triiger = aplicSourcemode()
  when(D === False){
    switch (mode){
      is(0, 1, 4, 5, 6, 7){
        triiger.assignFromBits(mode.resized)
      }
      default{
        triiger := aplicSourcemode.inactive
      }
    }
  }otherwise{
    triiger := aplicSourcemode.inactive
  }
  // doclaim doocmpletion
  // def doClaim(): Unit = ip := False
}

case class setState() extends Area {
  val setipnum = RegInit(B(0x0, 32 bits)).allowUnsetRegToAvoidLatch()
  val clripnum = RegInit(B(0x0, 32 bits)).allowUnsetRegToAvoidLatch()
  val setienum = RegInit(B(0x0, 32 bits))
  val clrienum = RegInit(B(0x0, 32 bits))

  // val setipflag = RegInit(False)
  // val setieflag = RegInit(False)
  // val clripflag = RegInit(False)
  // val clrieflag = RegInit(False)
}

// hartIds
case class idc(interrupts : Seq[APLICInterruptSource], id : Int) extends Bundle{
  val output = RegInit(False).allowUnsetRegToAvoidLatch()

  val idelivery = RegInit(False)
  val iforce = RegInit(False)
  val ithreshold = RegInit(B(0x0, 8 bits))
  // topi
  val topi_identity = RegInit(B(0x0, 10 bits))
  val topi_priority = RegInit(B(0x0, 8 bits))
  // claimi
  val claimi_identity = RegInit(B(0x0, 10 bits))
  val claimi_priority = RegInit(B(0x0, 8 bits))

  claimi_identity := topi_identity
  claimi_priority := topi_priority

  val generic = AIAGeneric(interrupts, id)
  generic.threshold := ithreshold.asUInt.resized

  when(generic.claim > 0){
    topi_identity := generic.claim.asBits.resized
    topi_priority := generic.bestRequest.asInstanceOf[APLICRequest].prio.asBits.resized
    output := True
  }otherwise{
    output := False
  }

}

// case class aplicGateway(input : Bool, id : UInt, source : source, domaincfg : domaincfg, childbits : Vec[Bits]) extends Area{
case class aplicGateway(input : Bool, idx : UInt, source : source, domaincfg : domaincfg, interrupt : AIAInterruptSource) extends Area{
  when(domaincfg.ie === True){
    when(source.D === True){
      // childbits(source.mode.asUInt.resized)(id) := input
      source.ie := False
    }otherwise {
      switch(source.triiger){
        is(aplicSourcemode.inactive){
          interrupt.ip := False
          source.ie := False
        }
        is(aplicSourcemode.detached){
          source.ie := True
        }
        is(aplicSourcemode.rising){
          when(input.rise()){
            source.ie := True
            interrupt.ip := True
          }
        }
        is(aplicSourcemode.falling){
          when(input.fall()){
            source.ie := True
            interrupt.ip := True
          }
        }
        is(aplicSourcemode.high){
          when(input === True){
            source.ie := True
            interrupt.ip := True
          }
        }
        is(aplicSourcemode.low){
          when(input === False){
            source.ie := True
            interrupt.ip := True
          }
        }
        // default{
        //   source.ip := False
        //   source.ie := False
        // }
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

// case class TilelinkAplic(sources : Int, hartIds : Int, p : bus.tilelink.BusParameter) extends MappedAplic[bus.tilelink.Bus](
//   sources,
//   hartIds,
//   new bus.tilelink.Bus(p),
//   new bus.tilelink.SlaveFactory(_, true)
// )

// object MyTopLevelVerilog extends App {
//   Config.spinal.generateVerilog(aplic())
// }

// object MyTopLevelVhdl extends App {
//   Config.spinal.generateVhdl(aplic())
// }
