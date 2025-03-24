package aplic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, AllMapping, SingleMapping}

case class aplicMapping(
  domaincfg 	: Int,
  sourcecfg 	: Int,
  mmsiaddrcfg 	: Int,
  mmsiaddrcfgh 	: Int,
  smsiaddrcfg 	: Int,
  smsiaddrcfgh 	: Int,
  setip 	: Int,
  setipnum 	: Int,
  in_clrip 	: Int,
  clripnum 	: Int,
  setie 	: Int,
  setienum 	: Int,
  clrie 	: Int,
  clrienum 	: Int,
  setipnum_le 	: Int,
  setipnum_be 	: Int,
  genmsi 	: Int,
  target 	: Int,
  idc 		: Int
)

object aplicMapping{
  def aplicMap = aplicMapping(
  domaincfg 	= 0x0000,
  sourcecfg 	= 0x0004,
  mmsiaddrcfg 	= 0x1BC0,
  mmsiaddrcfgh 	= 0x1BC4,
  smsiaddrcfg 	= 0x1BC8,
  smsiaddrcfgh 	= 0x1BCC,
  setip 	= 0x1C00,
  setipnum 	= 0x1CDC,
  in_clrip 	= 0x1D00,
  clripnum 	= 0x1DDC,
  setie 	= 0x1E00,
  setienum 	= 0x1EDC,
  clrie 	= 0x1F00,
  clrienum 	= 0x1FDC,
  setipnum_le 	= 0x2000,
  setipnum_be 	= 0x2004,
  genmsi 	= 0x3000,
  target 	= 0x3004,
  idc 		= 0x4000
  )
}

// object aplicMapper{
// 	def apply(bus: BusSlaveFactory, mapping: aplicMapping)(gateways : Seq[PlicGateway], targets : Seq[PlicTarget]) = new Area{
//     import mapping._


// 	}
// }
