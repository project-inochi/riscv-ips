package sim

import spinal.core._
import spinal.core.sim._

class SimUIntFix(width: Int)(number: BigInt, endian: Endianness = LITTLE) {
  val bits = width
  val value = number
  val endianness = endian
}

object SimUIntFix {
  implicit def simUintFix2SeqByte(intx: SimUIntFix): Seq[Byte] = {
    intx.value.toBytes(intx.bits, intx.endianness).toIndexedSeq
  }
}

object SimUInt8 {
  def apply(number: BigInt, endian: Endianness = LITTLE): SimUIntFix = new SimUIntFix(8)(number, endian)
}

object SimUInt16 {
  def apply(number: BigInt, endian: Endianness = LITTLE): SimUIntFix = new SimUIntFix(16)(number, endian)
}

object SimUInt32 {
  def apply(number: BigInt, endian: Endianness = LITTLE): SimUIntFix = new SimUIntFix(32)(number, endian)
}

object SimUInt64 {
  def apply(number: BigInt, endian: Endianness = LITTLE): SimUIntFix = new SimUIntFix(64)(number, endian)
}
