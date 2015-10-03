package longbenchmark.optteavmlike

import scala.annotation.tailrec

import scala.scalajs.js
import js.JSNumberOps._

class RuntimeLong(val lo: Int, val hi: Int) { a =>
  import RuntimeLong._
  import Build._

  def notEquals(b: RuntimeLong): Boolean =
    a.lo != b.lo || a.hi != b.hi

  override def toString(): String = {
    "(" + lo + ", " + hi + ")"
  }

  @inline
  private def inline_~ : RuntimeLong =
    new RuntimeLong(~lo, ~hi)

  @noinline
  def unary_~ : RuntimeLong = inline_~

  @inline
  private def inline_inc: RuntimeLong = {
    val lo = a.lo + 1
    new RuntimeLong(lo, hi + (if (lo == 0) 1 else 0))
  }

  @inline
  def inline_unary_-(lo: Int, hi: Int): (Int, Int) =
    (-lo, ~hi + (if (lo == 0) 1 else 0))

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

  @noinline
  def unary_- : RuntimeLong =
    fromPair(inline_unary_-(lo, hi))

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

    /*import js.DynamicImplicits.number2dynamic

    val Mask10 = (1 << 10) - 1
    val Mask12 = (1 << 12) - 1
    val Mask20 = (1 << 20) - 1
    val Mask22 = (1 << 22) - 1

    val a0 = (a.lo & Mask22).toDouble
    val a1 = ((a.lo >>> 22) | ((a.hi & Mask12) << 10)).toDouble
    val a2 = (a.hi >>> 12).toDouble

    val b0 = (b.lo & Mask22).toDouble
    val b1 = ((b.lo >>> 22) | ((b.hi & Mask12) << 10)).toDouble
    val b2 = (b.hi >>> 12).toDouble

    var p0: js.Dynamic = a0 * b0
    var p1: js.Dynamic = a1 * b0
    var p2: js.Dynamic = a2 * b0

    if (b1 != 0) {
      p1 = p1 + a0 * b1
      p2 = p2 + a1 * b1
    }
    if (b2 != 0) {
      p2 = p2 + a0 * b2
    }

    p1 = p1 + ((p0 / (1 << 22)) | 0)
    p2 = p2 + ((p1 / (1 << 22)) | 0)

    val c0 = (p0 & Mask22).asInstanceOf[Int]
    val c1 = (p1 & Mask22).asInstanceOf[Int]
    val c2 = (p2 & Mask20).asInstanceOf[Int]

    new RuntimeLong(
        c0 | (c1 << 22),
        (c1 >>> 10) | (c2 << 12))*/
  }

  def /(b: RuntimeLong): RuntimeLong = divMod(b)(0)
  def %(b: RuntimeLong): RuntimeLong = divMod(b)(1)

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

  def <(b: RuntimeLong): Boolean = {
    val ahi = a.hi
    val bhi = b.hi
    (ahi < bhi) || (ahi == bhi && a.lo < b.lo)
  }

  // helpers //

  @inline private def isZero = lo == 0 && hi == 0
  @inline private def isMinValue = hi == 0x80000000 && lo == 0
  @inline private def isNegative = hi < 0
  @inline private def abs = if (isNegative) -a else a

  def signum: RuntimeLong =
    if (isNegative) MinusOne else if (isZero) Zero else One

  def numberOfLeadingZeros: Int = {
    val hi = this.hi
    if (hi != 0) Integer.numberOfLeadingZeros(hi)
    else Integer.numberOfLeadingZeros(lo) + 32
  }

  def numberOfTrailingZeros: Int = {
    val lo = this.lo
    if (lo != 0) Integer.numberOfTrailingZeros(lo)
    else Integer.numberOfTrailingZeros(hi) + 32
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
    if (lo == 0 && isPowerOfTwo(hi)) log2OfPowerOfTwo(hi)
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
}
