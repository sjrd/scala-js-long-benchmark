package longbenchmark.optteavmlike

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
    if (isInt32) {
      lo.toString()
    } else if (isNegative) {
      // Check for MinValue, because its not negatable
      if (isMinValue) "-9223372036854775808"
      else "-" + longToString(-a)
    } else {
      longToString(a)
    }
  }

  @inline private def isInt32: Boolean = isInt32(lo, hi)

  @inline private def isInt32(lo: Int, hi: Int): Boolean = hi == (lo >> 31)

  private def longToString(a: RuntimeLong): String = {
    val tenPow9 = TenPow9 // local copy to access the companion module only once

    var v = a
    var acc = ""
    while (true) {
      val quotRem = v.divMod(tenPow9)
      val quot = quotRem(0)
      val rem = quotRem(1)

      val digits = rem.toInt.toString

      if (quot.isZero) {
        return digits + acc
      }

      val zeroPrefix =
        "000000000".jsSubstring(digits.length) // (9 - digits.length) zeros

      v = quot
      acc = zeroPrefix + digits + acc
    }

    throw new AssertionError("dead code")
  }

  // Conversions

  def toByte: Byte = lo.toByte
  def toShort: Short = lo.toShort
  def toChar: Char = lo.toChar
  def toInt: Int = lo
  def toLong: Long = this.asInstanceOf[Long]
  def toFloat: Float = toDouble.toFloat

  def toDouble: Double = {
    if (isNegative) {
      if (isMinValue) {
        -9223372036854775808.0
      } else {
        val (abslo, abshi) = inline_unary_-(lo, hi)
        -(abslo + abshi * TwoPow32)
      }
    } else {
      lo + hi * TwoPow32
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
      a.lo.compareTo(b.lo)
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
    if (ahi == bhi) a.lo.toUint < b.lo.toUint
    else ahi < bhi
  }

  def <=(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) a.lo.toUint <= b.lo.toUint
    else ahi < bhi
  }

  def >(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) a.lo.toUint > b.lo.toUint
    else ahi > bhi
  }

  def >=(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    if (ahi == bhi) a.lo.toUint >= b.lo.toUint
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

  /** Logical shift right */
  def >>>(n0: Int): RuntimeLong = {
    val n = n0 & 63
    val hi = this.hi

    if (n == 0) this
    else if (n < 32) new RuntimeLong((lo >>> n) | (hi << -n), hi >>> n)
    else new RuntimeLong(hi >>> n, 0)
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
  private def inline_inc: RuntimeLong = {
    val lo = a.lo + 1
    new RuntimeLong(lo, hi + (if (lo == 0) 1 else 0))
  }

  def +(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val blo = b.lo
    val lo = alo + blo
    val hi = a.hi + b.hi + (if (lo.toUint < alo.toUint) 1 else 0)
    new RuntimeLong(lo, hi)
  }

  def -(b: RuntimeLong): RuntimeLong = {
    val alo = a.lo
    val blo = b.lo
    val lo = alo - blo
    val hi = a.hi - b.hi + (if (lo.toUint > alo.toUint) -1 else 0)
    new RuntimeLong(lo, hi)
  }

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
        if (alo == Int.MinValue && blo == -1) a
        else new RuntimeLong(alo / blo)
      } else {
        // Either a == Int.MinValue && b == (Int.MaxValue + 1), or (abs(b) > abs(a))
        if (alo == Int.MinValue && (blo == 0x80000000 && bhi == 0)) One
        else Zero // because abs(b) > abs(a)
      }
    } else {
      val aPos = ahi >= 0
      val aAbsHi = if (aPos) ahi else if (alo != 0) ~ahi else -ahi
      if ((aAbsHi & 0xffe00000) == 0) {
        // abs(a) < 2^53, so it's a safe Double
        val bPos = bhi >= 0
        val bAbsHi = if (bPos) bhi else if (blo != 0) ~bhi else -bhi
        if ((bAbsHi & 0xffe00000) == 0) {
          // oh, abs(b) too!
          val aAbsLo = if (aPos) alo else -alo
          val bAbsLo = if (bPos) blo else -blo
          val aAbsDouble = aAbsHi * TwoPow32 + aAbsLo.toUint
          val bAbsDouble = bAbsHi * TwoPow32 + bAbsLo.toUint
          val rAbsDouble = aAbsDouble / bAbsDouble
          val rAbsLo = rawToInt(rAbsDouble)
          val rAbsHi = rawToInt(rAbsDouble / TwoPow32)
          if (aPos == bPos) new RuntimeLong(rAbsLo, rAbsHi)
          else fromPair(inline_unary_-(rAbsLo, rAbsHi))
        } else {
          Zero // because abs(b) > abs(a)
        }
      } else {
        // abs(a) is not a safe Double, we'll have to use the very slow path
        divMod(b)(0)
      }
    }
  }

  def %(b: RuntimeLong): RuntimeLong = {
    divMod(b)(1)
  }

  // helpers //

  @inline private def isZero = lo == 0 && hi == 0
  @inline private def isMinValue = hi == 0x80000000 && lo == 0
  @inline private def isNegative = hi < 0
  @inline private def abs = if (isNegative) -a else a

  @inline private def numberOfLeadingZeros: Int = {
    val hi = this.hi
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else Integer.numberOfLeadingZeros(lo) + 32
  }

  /** return log_2(x) if power of 2 or -1 otherwise */
  private def powerOfTwo = {
    @inline def isPowerOfTwo(i: Int): Boolean =
      i != 0 && (i & (i - 1)) == 0
    @inline def log2OfPowerOfTwo(i: Int): Int =
      Integer.numberOfTrailingZeros(i)

    val lo = this.lo
    val hi = this.hi

    if (hi == 0 && isPowerOfTwo(lo)) log2OfPowerOfTwo(lo)
    else if (lo == 0 && isPowerOfTwo(hi)) log2OfPowerOfTwo(hi)
    else -1
  }

  private def setBit(bit: Int): RuntimeLong =
    if (bit < 32) new RuntimeLong(lo | (1 << bit), hi)
    else new RuntimeLong(lo, hi | (1 << (bit - 32)))

  private def divMod(y: RuntimeLong): scala.scalajs.js.Array[RuntimeLong] = {
    // scalastyle:off return
    import scala.scalajs.js
    val x = this
    if (y.isZero) {
      throw new ArithmeticException("/ by zero")
    } else if (x.isZero) {
      js.Array(Zero, Zero)
    } else if (y.isMinValue) {
      // MinValue / MinValue == 1, rem = 0
      // otherwise == 0, rem x
      if (x.isMinValue) js.Array(One, Zero)
      else js.Array(Zero, x)
    } else {
      val xNegative = x.isNegative
      val yNegative = y.isNegative

      val xMinValue = x.isMinValue

      val pow = y.powerOfTwo
      if (pow >= 0) {
        if (xMinValue) {
          val z = x >> pow
          js.Array(if (yNegative) -z else z, Zero)
        } else {
          // x is not min value, so we can calculate absX
          val absX = x.abs
          val absZ = absX >> pow
          val z = if (xNegative ^ yNegative) -absZ else absZ
          val remAbs = absX.maskRight(pow)
          val rem = if (xNegative) -remAbs else remAbs
          js.Array(z, rem)
        }
      } else {
        val absY = y.abs

        val newX = {
          if (xMinValue) {
            MaxValue
          } else {
            val absX = x.abs
            if (absX < absY)
              return js.Array(Zero, x) // <-- ugly but fast
            else
              absX
          }
        }
        divModHelper(newX, absY, xNegative, yNegative, xMinValue)
      }
    }
    // scalastyle:on return
  }

  @inline
  private def maskRight(bits: Int): RuntimeLong =
    if (bits <= 32) new RuntimeLong(lo & ((1 << bits) - 1), 0)
    else new RuntimeLong(lo, hi & ((1 << bits) - 1)) // 1 << (bits - 32)

  /**
   * performs division in "normal cases"
   * @param x absolute value of the numerator
   * @param y absolute value of the denominator
   * @param xNegative whether numerator was negative
   * @param yNegative whether denominator was negative
   * @param xMinValue whether numerator was Long.minValue
   */
  @inline
  private def divModHelper(x: RuntimeLong, y: RuntimeLong,
      xNegative: Boolean, yNegative: Boolean,
      xMinValue: Boolean): scala.scalajs.js.Array[RuntimeLong] = {
    import scala.scalajs.js

    var shift = y.numberOfLeadingZeros - x.numberOfLeadingZeros
    var yShift = y << shift
    var absRem = x
    var absQuot = Zero
    while (shift >= 0 && !absRem.isZero) {
      val newX = absRem - yShift
      if (!newX.isNegative) {
        absRem = newX
        absQuot = absQuot.setBit(shift)
      }
      shift -= 1
      yShift >>= 1
    }

    val quot = if (xNegative ^ yNegative) -absQuot else absQuot
    val rem  =
      if (xNegative && xMinValue) -absRem - One
      else if (xNegative)         -absRem
      else                         absRem

    js.Array(quot, rem)
  }
}

object RuntimeLong {
  final val Zero = new RuntimeLong(0, 0)
  final val One = new RuntimeLong(1, 0)
  final val MinusOne = new RuntimeLong(-1, -1)
  final val MinValue = new RuntimeLong(0, 0x80000000)
  final val MaxValue = new RuntimeLong(0xffffffff, 0x7fffffff)
  final val TenPow9 = new RuntimeLong(1000000000, 0) // 1000000000L with 9 zeros
}
