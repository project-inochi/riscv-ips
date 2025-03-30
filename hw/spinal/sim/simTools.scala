package sim

import spinal.core._
import spinal.core.sim._
import config.Config

object simTools{
  def UInt32(data : Int, align : Endianness = LITTLE): Array[Byte] = BigInt(data).toBytes(bits = 32, endian = align)
}
