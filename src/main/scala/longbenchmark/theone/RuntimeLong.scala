package longbenchmark.theone

import scala.annotation.tailrec

import scala.scalajs.js
import js.JSNumberOps._
import js.JSStringOps._

class RuntimeLong(val lo: Int, val hi: Int)
    extends java.lang.Number with java.io.Serializable
    with java.lang.Comparable[java.lang.Long] { a =>

  import RuntimeLong._
  import Build._

  /** Constructs a Long from an Int. */
  def this(value: Int) = this(value, value >> 31)

  // Universal equality

  override def equals(that: Any): Boolean = that match {
    case b: RuntimeLong => inline_equals(b)
    case _              => false
  }

  override def hashCode(): Int =
    lo ^ hi

  // String operations

  override def toString(): String = {
    val lo = this.lo
    val hi = this.hi

    if (isInt32(lo, hi)) {
      lo.toString()
    } else if (hi < 0) {
      val (absLo, absHi) = inline_unary_-(lo, hi)
      "-" + toUnsignedString(absLo, absHi)
    } else {
      toUnsignedString(lo, hi)
    }
  }

  @inline private def isInt32: Boolean = isInt32(lo, hi)

  @inline private def isInt32(lo: Int, hi: Int): Boolean = hi == (lo >> 31)

  private def toUnsignedString(lo: Int, hi: Int): String = {
    // This is called only if (lo, hi) is not an Int32

    if ((hi & 0xffe00000) == 0) {
      // (lo, hi) is small enough to be a Double, use that directly
      val value = hi * TwoPow32 + lo.toUint
      value.toString
    } else {
      /* We divide (lo, hi) once by 2^10 and keep the remainder.
       *
       * The remainder must then be < 2^10, and is therefore an int32.
       *
       * The quotient must be <= ULong.MaxValue / 10^9, which is < 2^53, and
       * is therefore a valid double. It must also be non-zero, since we tested
       * previously for cases where (lo, hi) < 2^53, but 2^10 is itself < 2^53.
       */
      val TenPow9Lo = 1000000000L.toInt
      val TenPow9Hi = (1000000000L >>> 32).toInt

      val (quotLo, quotHi, rem, _) = // remHi must be 0 by construction
        unsignedDivModHelper(lo, hi, TenPow9Lo, TenPow9Hi)

      val quot = quotHi * TwoPow32 + quotLo.toUint

      val remStr = rem.toString
      quot.toString + "000000000".jsSubstring(remStr.length) + remStr
    }
  }

  // Conversions

  def toByte: Byte = lo.toByte
  def toShort: Short = lo.toShort
  def toChar: Char = lo.toChar
  def toInt: Int = lo
  def toLong: Long = this.asInstanceOf[Long]
  def toFloat: Float = toDouble.toFloat

  def toDouble: Double = {
    val lo = this.lo
    val hi = this.hi

    if (hi < 0) {
      val (abslo, abshi) = inline_unary_-(lo, hi)
      -(abshi.toUint * TwoPow32 + abslo.toUint) // abshi.toUint for MinValue
    } else {
      hi * TwoPow32 + lo.toUint
    }
  }

  // java.lang.Number

  override def byteValue(): Byte = toByte
  override def shortValue(): Short = toShort
  def intValue(): Int = toInt
  def longValue(): Long = toLong
  def floatValue(): Float = toFloat
  def doubleValue(): Double = toDouble

  // Comparisons and java.lang.Comparable interface

  def compareTo(b: RuntimeLong): Int = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) {
      val alo = a.lo
      val blo = b.lo
      if (alo == blo) 0
      else if (inlineUnsignedInt_<(alo, blo)) -1
      else 1
    } else {
      if (ahi < bhi) -1
      else 1
    }
  }

  def compareTo(that: java.lang.Long): Int =
    compareTo(that.asInstanceOf[RuntimeLong])

  @inline
  private def inline_equals(b: RuntimeLong): Boolean =
    a.lo == b.lo && a.hi == b.hi

  @noinline
  def equals(b: RuntimeLong): Boolean = inline_equals(b)

  @noinline
  def notEquals(b: RuntimeLong): Boolean = !inline_equals(b)

  def <(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_<(a.lo, b.lo)
    else ahi < bhi
  }

  def <=(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_<=(a.lo, b.lo)
    else ahi < bhi
  }

  def >(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_>(a.lo, b.lo)
    else ahi > bhi
  }

  def >=(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) inlineUnsignedInt_>=(a.lo, b.lo)
    else ahi > bhi
  }

  // Logic/bitwise operations

  @inline
  private def inline_~ : RuntimeLong =
    new RuntimeLong(~lo, ~hi)

  @noinline
  def unary_~ : RuntimeLong = inline_~

  def |(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo | b.lo, a.hi | b.hi)

  def &(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo & b.lo, a.hi & b.hi)

  def ^(b: RuntimeLong): RuntimeLong =
    new RuntimeLong(a.lo ^ b.lo, a.hi ^ b.hi)

  /** Shift left */
  def <<(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val lo = this.lo

    if (n == 0) this
    else if (n < 32) new RuntimeLong(lo << n, (lo >>> -n) | (hi << n))
    else new RuntimeLong(0, lo << n)
  }

  @inline
  def inline_<<(lo: Int, hi: Int, n: Int): (Int, Int) = {
    if (n == 0) (lo, hi)
    else if (n < 32) (lo << n, (lo >>> -n) | (hi << n))
    else (0, lo << n)
  }

  /** Logical shift right */
  def >>>(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val hi = this.hi

    if (n == 0) this
    else if (n < 32) new RuntimeLong((lo >>> n) | (hi << -n), hi >>> n)
    else new RuntimeLong(hi >>> n, 0)
  }

  @inline
  def inline_>>>(lo: Int, hi: Int, n: Int): (Int, Int) = {
    if (n == 0) (lo, hi)
    else if (n < 32) ((lo >>> n) | (hi << -n), hi >>> n)
    else (hi >>> n, 0)
  }

  /** Arithmetic shift right */
  def >>(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val hi = this.hi

    if (n == 0) this
    else if (n < 32) new RuntimeLong((lo >>> n) | (hi << -n), hi >> n)
    else new RuntimeLong(hi >> n, hi >> 31)
  }

  // Arithmetic operations

  @noinline
  def unary_+ : RuntimeLong = this

  @noinline
  def unary_- : RuntimeLong =
    fromPair(inline_unary_-(lo, hi))

  @inline
  def inline_unary_-(lo: Int, hi: Int): (Int, Int) =
    (-lo, if (lo != 0) ~hi else -hi)

  @inline
  def inline_abs(lo: Int, hi: Int): (Boolean, Int, Int) = {
    val neg = hi < 0
    var absLo = lo
    var absHi = hi
    if (neg) {
      absLo = -lo
      absHi = if (lo != 0) ~hi else -hi
    }
    (neg, absLo, absHi)
  }

  @inline
  private def inline_inc: RuntimeLong = {
    val lo = a.lo + 1
    new RuntimeLong(lo, hi + (if (lo == 0) 1 else 0))
  }

  def +(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val blo = b.lo
    val lo = alo + blo
    val hi = a.hi + b.hi + (if (inlineUnsignedInt_<(lo, alo)) 1 else 0)
    new RuntimeLong(lo, hi)
  }

  @inline
  def inline_+(alo: Int, ahi: Int, blo: Int, bhi: Int): (Int, Int) = {
    val lo = alo + blo
    (lo, ahi + bhi + (if (inlineUnsignedInt_<(lo, alo)) 1 else 0))
  }

  def -(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val blo = b.lo
    val lo = alo - blo
    val hi = a.hi - b.hi + (if (inlineUnsignedInt_>(lo, alo)) -1 else 0)
    new RuntimeLong(lo, hi)
  }

  @inline
  def inline_-(alo: Int, ahi: Int, blo: Int, bhi: Int): (Int, Int) = {
    val lo = alo - blo
    (lo, ahi - bhi + (if (inlineUnsignedInt_>(lo, alo)) -1 else 0))
  }

  @inline
  def inlineUnsignedInt_<(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) < (b ^ 0x80000000)

  @inline
  def inlineUnsignedInt_<=(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) <= (b ^ 0x80000000)

  @inline
  def inlineUnsignedInt_>(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) > (b ^ 0x80000000)

  @inline
  def inlineUnsignedInt_>=(a: Int, b: Int): Boolean =
    (a ^ 0x80000000) >= (b ^ 0x80000000)

  def *(b: RuntimeLong): RuntimeLong = {
    val a0 = a.lo & 0xFFFF
    val a1 = a.lo >>> 16
    val a2 = a.hi & 0xFFFF
    val a3 = a.hi >>> 16
    val b0 = b.lo & 0xFFFF
    val b1 = b.lo >>> 16
    val b2 = b.hi & 0xFFFF
    val b3 = b.hi >>> 16

    var c0 = a0 * b0

    var c1 = c0 >>> 16
    c1 = c1 + a1 * b0

    var c2 = c1 >>> 16
    c1 = (c1 & 0xFFFF) + a0 * b1
    c2 = c2 + (c1 >>> 16)

    var c3 = c2 >>> 16
    c2 = (c2 & 0xFFFF) + a2 * b0
    c3 = c3 + (c2 >>> 16)
    c2 = (c2 & 0xFFFF) + a1 * b1
    c3 = c3 + (c2 >>> 16)
    c2 = (c2 & 0xFFFF) + a0 * b2
    c3 = c3 + (c2 >>> 16)
    c3 = c3 + a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

    new RuntimeLong(
        (c0 & 0xFFFF) | (c1 << 16),
        (c2 & 0xFFFF) | (c3 << 16))
  }

  def /(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val blo = b.lo
    val bhi = b.hi

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        if (alo == Int.MinValue && blo == -1) new RuntimeLong(Int.MinValue, 0)
        else new RuntimeLong(alo / blo)
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) One
        else Zero // because abs(b) > abs(a)
      }
    } else {
      val (aNeg, aAbsLo, aAbsHi) = inline_abs(alo, ahi)
      val (bNeg, bAbsLo, bAbsHi) = inline_abs(blo, bhi)
      val absR = unsigned_/(aAbsLo, aAbsHi, bAbsLo, bAbsHi)
      if (aNeg == bNeg) absR
      else fromPair(inline_unary_-(absR.lo, absR.hi))
    }
  }

  def unsigned_/(alo: Int, ahi: Int, blo: Int, bhi: Int): RuntimeLong = {
    if (blo == 0 && bhi == 0)
      throw new ArithmeticException("/ by zero")

    if ((ahi & 0xffe00000) == 0) {
      if ((bhi & 0xffe0000) == 0) {
        val aDouble = ahi * TwoPow32 + alo.toUint
        val bDouble = bhi * TwoPow32 + blo.toUint
        val rDouble = aDouble / bDouble
        new RuntimeLong(rawToInt(rDouble), rawToInt(rDouble / TwoPow32))
      } else {
        Zero // because b > a
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        val pow = log2OfPowerOfTwo(blo)
        if (pow == 0) new RuntimeLong(alo, ahi)
        else new RuntimeLong((alo >>> pow) | (ahi << -pow), ahi >>> pow)
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        val pow = log2OfPowerOfTwo(bhi)
        new RuntimeLong(ahi >>> pow, 0)
      } else {
        @noinline
        def slowPath() = {
          val (lo, hi, _, _) = unsignedDivModHelper(alo, ahi, blo, bhi)
          new RuntimeLong(lo, hi)
        }
        slowPath()
      }
    }
  }

  def %(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val ahi = a.hi
    val blo = b.lo
    val bhi = b.hi

    if (isInt32(alo, ahi)) {
      if (isInt32(blo, bhi)) {
        new RuntimeLong(alo % blo)
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) Zero
        else a // because abs(b) > abs(a)
      }
    } else {
      val (aNeg, aAbsLo, aAbsHi) = inline_abs(alo, ahi)
      val (bNeg, bAbsLo, bAbsHi) = inline_abs(blo, bhi)
      val absR = unsigned_%(aAbsLo, aAbsHi, bAbsLo, bAbsHi)
      if (aNeg) fromPair(inline_unary_-(absR.lo, absR.hi))
      else absR
    }
  }

  def unsigned_%(alo: Int, ahi: Int, blo: Int, bhi: Int): RuntimeLong = {
    if (blo == 0 && bhi == 0)
      throw new ArithmeticException("/ by zero")

    if ((ahi & 0xffe00000) == 0) {
      if ((bhi & 0xffe0000) == 0) {
        val aDouble = ahi * TwoPow32 + alo.toUint
        val bDouble = bhi * TwoPow32 + blo.toUint
        val rDouble = aDouble % bDouble
        new RuntimeLong(rawToInt(rDouble), rawToInt(rDouble / TwoPow32))
      } else {
        new RuntimeLong(alo, ahi) // because b > a
      }
    } else {
      if (bhi == 0 && isPowerOfTwo_IKnowItsNot0(blo)) {
        val pow = log2OfPowerOfTwo(blo)
        new RuntimeLong(alo & ((1 << pow) - 1), 0)
      } else if (blo == 0 && isPowerOfTwo_IKnowItsNot0(bhi)) {
        val pow = log2OfPowerOfTwo(bhi)
        new RuntimeLong(alo, ahi & ((1 << pow) - 1))
      } else {
        @noinline
        def slowPath() = {
          val (_, _, lo, hi) = unsignedDivModHelper(alo, ahi, blo, bhi)
          new RuntimeLong(lo, hi)
        }
        slowPath()
      }
    }
  }

  @inline
  private def unsignedDivModHelper(alo: Int, ahi: Int, blo: Int,
      bhi: Int): (Int, Int, Int, Int) = {

    var shift =
      inlineNumberOfLeadingZeros(blo, bhi) - inlineNumberOfLeadingZeros(alo, ahi)
    val initialBShift = inline_<<(blo, bhi, shift)
    var bShiftLo = initialBShift._1
    var bShiftHi = initialBShift._2
    var remLo = alo
    var remHi = ahi
    var quotLo = 0
    var quotHi = 0

    /* Invariants:
     *   bShift == b << shift == b * 2^shift
     *   quot >= 0
     *   0 <= rem < 2 * bShift
     *   quot * b + rem == a
     */
    //while (shift >= 0 && (remLo != 0 || remHi != 0)) {
    while (shift >= 0 && (remHi & 0xffe00000) != 0) {
      if (inlineUnsigned_>=(remLo, remHi, bShiftLo, bShiftHi)) {
        val newRem = inline_-(remLo, remHi, bShiftLo, bShiftHi)
        remLo = newRem._1
        remHi = newRem._2
        if (shift < 32)
          quotLo |= (1 << shift)
        else
          quotHi |= (1 << shift) // == (1 << (shift - 32))
      }
      shift -= 1
      val newBShift = inline_>>>(bShiftLo, bShiftHi, 1)
      bShiftLo = newBShift._1
      bShiftHi = newBShift._2
    }

    // Now rem < 2^53, we can finish with a double division
    if (inlineUnsigned_>=(remLo, remHi, blo, bhi)) {
      val remDouble = remHi * TwoPow32 + remLo.toUint
      val bDouble = bhi * TwoPow32 + blo.toUint

      val rem_div_bDouble = remDouble / bDouble
      val newQuot = inline_+(quotLo, quotHi,
          rawToInt(rem_div_bDouble), rawToInt(rem_div_bDouble / TwoPow32))
      quotLo = newQuot._1
      quotHi = newQuot._2

      val rem_mod_bDouble = remDouble % bDouble
      remLo = rawToInt(rem_mod_bDouble)
      remHi = rawToInt(rem_mod_bDouble / TwoPow32)
    }

    (quotLo, quotHi, remLo, remHi)
  }

  @inline
  def inlineUnsigned_>=(alo: Int, ahi: Int, blo: Int, bhi: Int): Boolean = {
    if (ahi == bhi)
      (alo ^ 0x80000000) >= (blo ^ 0x80000000)
    else
      (ahi ^ 0x80000000) >= (bhi ^ 0x80000000)
  }

  // helpers //

  @inline private def isPowerOfTwo(i: Int): Boolean =
    i != 0 && isPowerOfTwo_IKnowItsNot0(i)
  @inline private def isPowerOfTwo_IKnowItsNot0(i: Int): Boolean =
    (i & (i - 1)) == 0
  @inline private def log2OfPowerOfTwo(i: Int): Int =
    Integer.numberOfTrailingZeros(i)

  @inline private def inlineNumberOfLeadingZeros(lo: Int, hi: Int): Int =
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else Integer.numberOfLeadingZeros(lo) + 32
}

object RuntimeLong {
  final val Zero = new RuntimeLong(0, 0)
  final val One = new RuntimeLong(1, 0)
}
