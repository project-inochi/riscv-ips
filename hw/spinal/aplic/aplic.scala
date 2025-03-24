package aplic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import scala.tools.nsc.doc.html.HtmlTags.Tr
import scala.collection.Seq
import spinal.lib.bus.amba4.axilite.AxiLite4SlaveFactory

class MappedAplic[T <: spinal.core.Data with IMasterSlave](sourceIds : Seq[Int],
                                                           hartIds : Seq[Int],
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component{
  val io = new Bundle {
    val bus = slave(busType())
    val sources = in Bits (sourceIds.size bits)
    val targets = out Bits (hartIds.size bits)
    val child0 = out Bits (sourceIds.size bits)
  }

  val child = Vec(io.child0)

  val domaincfg = new domaincfg()
  val setState = new setState()

  // sourceids
  val sources = for (sourceId <- sourceIds) yield new source(sourceId)
  val gateways = for ((source, idx) <- sources.zipWithIndex) yield new aplicGateway(io.sources(idx), idx, source, domaincfg, child)
  // hartids
  val idcs = for (hartId <- hartIds) yield new idc(sources, hartId)

  io.targets := idcs.map(_.target).asBits

  // TODO:
  // val bus = new AxiLite4SlaveFactory(io.bus)
  // val mapping = PlicMapper(bus, plicMapping)(
  //   gateways = gateways,
  //   targets = targets
  // )


  /*TODO:
   * 1. setstate
   * 2. bus寄存器
   * 3. output分配
   * 4. threshold
   */
}

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

case class domaincfg() extends Bundle {
  val align = RegInit(B(0x80, 8 bits))
  val ie = RegInit(False)
  val dm = RegInit(False)
  val be = RegInit(False)
}

// sourceIds
case class source(id : UInt) extends Bundle {
  val idx = RegInit(id)
  // sourcecfg
  val D = RegInit(False)
  val mode = RegInit(B(0x0, 10 bits))
  // setip
  val ie = RegInit(False)
  val ip = RegInit(False)

  val valid = ie && ip
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
        triiger.assignFromBits(mode)
      }
      default{
        triiger := aplicSourcemode.inactive
      }
    }
  }otherwise{
    triiger := aplicSourcemode.inactive
  }
}

case class setState() extends Area {
  val setipnum = RegInit(B(0x0, 32 bits))
  val clripnum = RegInit(B(0x0, 32 bits))
  val setienum = RegInit(B(0x0, 32 bits))
  val clrienum = RegInit(B(0x0, 32 bits))
}

// hartIds
case class idc(sources : Seq[source], id : UInt) extends Bundle{
  val target = RegInit(False)
  val idx = RegInit(id)

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
  claimi_identity := topi_priority

  case class Request(id : UInt, priority : Bits, Valid : Bool) extends Bundle{
    val idx = UInt(10 bits)
    val iprio = UInt(8 bits)
    val valid = Bool()
  }

  val requests = for (source <- sources) yield new Request(source.idx, source.iprio,
                                                           source.valid &&
                                                           source.hartindex.asUInt === idx)

  val bestRequest = RegNext(requests.reduceBalancedTree((a, b) => {
    val takeA = !b.valid || (a.valid && a.iprio <= b.iprio)
    takeA ? a | b
  }))

  when(bestRequest.valid === True){
    val iep = bestRequest.iprio < topi_priority.asUInt ||
              (bestRequest.iprio === topi_priority.asUInt &&
               bestRequest.idx < topi_identity.asUInt)
    when(iep === True){
      topi_identity := bestRequest.idx.asBits
      topi_priority := bestRequest.iprio.asBits

      // idcs(bestRequest.idx).claimi_identity := bestRequest.idx.asBits
      // idcs(bestRequest.idx).claimi_priority := bestRequest.iprio
    }
    target := True
  }otherwise{
    target := False
  }
}

case class aplicGateway(input : Bool, id : UInt, source : source, domaincfg : domaincfg, child : Vec[Bits]) extends Area{
  when(domaincfg.ie === True){
    when(source.D === True){
          child(source.mode.asUInt)(id) := input
          source.ie := False
    }otherwise {
      switch(source.triiger){
        is(aplicSourcemode.inactive){
          source.ip := False
          source.ie := False
        }
        is(aplicSourcemode.detached){
          source.ie := True
        }
        is(aplicSourcemode.rising){
          when(input.rise()){
            source.ie := True
            source.ip := True
          }
        }
        is(aplicSourcemode.falling){
          when(input.fall()){
            source.ie := True
            source.ip := True
          }
        }
        is(aplicSourcemode.high){
          when(input === True){
            source.ie := True
            source.ip := True
          }
        }
        is(aplicSourcemode.low){
          when(input === False){
            source.ie := True
            source.ip := True
          }
        }
        default{
          source.ip := False
          source.ie := False
        }
      }
    }
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
