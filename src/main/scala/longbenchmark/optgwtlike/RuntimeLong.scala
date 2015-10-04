package longbenchmark.optgwtlike

import scala.annotation.tailrec

import scala.scalajs.js
import js.DynamicImplicits.number2dynamic

/**
 * emulate a Java-Long using three integers.
 * taken from gwt LongLib:
 * com.google.gwt.lang.LongLib
 *
 * only used by runtime
 *
 * holds values l, m, h (low, middle, high)
 * s.t. (x.l + ((long) x.m << 22) + ((long) x.h << 44)) is equal to
 * the original value
 */
final class RuntimeLong(
  val l: Int,
  val m: Int,
  val h: Int
) extends Number with Comparable[java.lang.Long] { x =>

  import RuntimeLong._

  /** Construct from an Int.
   *  This is the implementation of RuntimeLong.fromInt() in a way that does not
   *  require to load to module RuntimeLong.
   */
  def this(value: Int) = this(
      value & RuntimeLong.MASK,
      (value >> RuntimeLong.BITS) & RuntimeLong.MASK,
      if (value < 0) RuntimeLong.MASK_2 else 0)

  /** Creates a new RuntimeLong but masks bits as follows:
   *  l & MASK, m & MASK, h & MASK_2
   */
  @inline private def masked(l: Int, m: Int, h: Int) =
    new RuntimeLong(l & MASK, m & MASK, h & MASK_2)

  def toByte: Byte = toInt.toByte
  def toShort: Short = toInt.toShort
  def toChar: Char = toInt.toChar
  def toInt: Int = l | (m << BITS)
  def toLong: Long = x.asInstanceOf[Long]
  def toFloat: Float = toDouble.toFloat
  def toDouble: Double =
    if (isMinValue) -9223372036854775808.0
    else if (isNegative) -((-x).toDouble)
    else l + m * TWO_PWR_22_DBL + h * TWO_PWR_44_DBL

  // java.lang.Number
  override def byteValue(): Byte = toByte
  override def shortValue(): Short = toShort
  def intValue(): Int = toInt
  def longValue(): Long = toLong
  def floatValue(): Float = toFloat
  def doubleValue(): Double = toDouble

  // java.lang.Comparable + overload taking scala.Long
  def compareTo(that: RuntimeLong): Int =
    if (this equals that) 0 else if (this > that) 1 else -1
  def compareTo(that: java.lang.Long): Int =
    compareTo(that.asInstanceOf[RuntimeLong])

  // scalastyle:off disallow.space.before.token
  def unary_~ : RuntimeLong = masked(~x.l, ~x.m, ~x.h)
  def unary_+ : RuntimeLong = x
  def unary_- : RuntimeLong = {
    val neg0 = (~x.l + 1) & MASK
    val neg1 = (~x.m + (if (neg0 == 0) 1 else 0)) & MASK
    val neg2 = (~x.h + (if (neg0 == 0 && neg1 == 0) 1 else 0)) & MASK_2
    new RuntimeLong(neg0, neg1, neg2)
  }
  // scalastyle:on disallow.space.before.token

  def +(y: String): String = x.toString + y

  def <<(n_in: Int): RuntimeLong = {
    /* crop MSB. Note: This will cause (2L << 65 == 2L << 1)
     * apparently this is as specified
     */
    val n = n_in & 63

    if (n < BITS) {
      val remBits = BITS - n
      masked(x.l << n,
             (x.m << n) | (x.l >> remBits),
             (x.h << n) | (x.m >> remBits))
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
      masked(0, x.l << shfBits, (x.m << shfBits) | (x.l >> remBits))
    } else {
      masked(0, 0, x.l << (n - BITS01))
    }

  }

  /**
   * logical right shift
   */
  def >>>(n_in: Int): RuntimeLong = {
    val n = n_in & 63
    if (n < BITS) {
      val remBits = BITS - n
      masked((x.l >> n) | (x.m << remBits),
             // FIXME is this really >> and not >>>??
             (x.m >> n) | (x.h << remBits),
             x.h >>> n)
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
             // FIXME is this really >> and not >>>??
      masked((x.m >> shfBits) | (x.h << remBits),
             x.h >>> shfBits, 0)
    } else {
      masked(x.h >>> (n - BITS01), 0, 0)
    }
  }

  /**
   * arithmetic right shift
   */
  def >>(n_in: Int): RuntimeLong = {
    val n = n_in & 63;

    // Sign extend x.h
    val negative = (x.h & SIGN_BIT_VALUE) != 0
    val xh = if (negative) x.h | ~MASK_2 else x.h

    if (n < BITS) {
      val remBits = BITS - n
      // FIXME IMHO the first two >> should be >>>
      masked((x.l >> n) | (x.m << remBits),
             (x.m >> n) | (xh  << remBits),
             xh >> n)
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
      // FIXME IMHO the first >> should be >>>
      masked((x.m >> shfBits) | (xh << remBits),
              xh  >> shfBits,
             if (negative) MASK_2 else 0)
    } else {
      masked(xh >> (n - BITS01),
             if (negative) MASK   else 0,
             if (negative) MASK_2 else 0)
    }

  }

  def equals(y: RuntimeLong): Boolean =
    x.l == y.l && x.m == y.m && x.h == y.h

  override def equals(that: Any): Boolean = that match {
    case y: RuntimeLong => x.equals(y)
    case _ => false
  }

  def notEquals(that: RuntimeLong): Boolean = !equals(that)

  override def hashCode(): Int = {
    (this ^ (this >>> 32)).toInt
  }

  @inline
  def <(y: RuntimeLong): Boolean = y > x
  @inline
  def <=(y: RuntimeLong): Boolean = y >= x

  def >(y: RuntimeLong): Boolean = {
    if (!x.isNegative)
      y.isNegative ||
      x.h >  y.h ||
      x.h == y.h && x.m >  y.m ||
      x.h == y.h && x.m == y.m && x.l >  y.l
    else !(
      !y.isNegative ||
      x.h <  y.h ||
      x.h == y.h && x.m <  y.m ||
      x.h == y.h && x.m == y.m && x.l <= y.l
    )
  }

  def >=(y: RuntimeLong): Boolean = {
    if (!x.isNegative)
      y.isNegative ||
      x.h >  y.h ||
      x.h == y.h && x.m >  y.m ||
      x.h == y.h && x.m == y.m && x.l >= y.l
    else !(
      !y.isNegative ||
      x.h <  y.h ||
      x.h == y.h && x.m <  y.m ||
      x.h == y.h && x.m == y.m && x.l <  y.l
    )
  }

  def |(y: RuntimeLong): RuntimeLong =
    new RuntimeLong(x.l | y.l, x.m | y.m, x.h | y.h)
  def &(y: RuntimeLong): RuntimeLong =
    new RuntimeLong(x.l & y.l, x.m & y.m, x.h & y.h)
  def ^(y: RuntimeLong): RuntimeLong =
    new RuntimeLong(x.l ^ y.l, x.m ^ y.m, x.h ^ y.h)

  def +(y: RuntimeLong): RuntimeLong = {
    val sum0 = x.l + y.l
    val sum1 = x.m + y.m + (sum0 >> BITS)
    val sum2 = x.h + y.h + (sum1 >> BITS)
    masked(sum0, sum1, sum2)
  }

  def -(y: RuntimeLong): RuntimeLong = {
    val neg0 = x.l - y.l
    val neg1 = x.m - y.m + (neg0 >> 31)
    val neg2 = x.h - y.m + (neg1 >> 31)
    masked(neg0, neg1, neg2)
  }

  def *(y: RuntimeLong): RuntimeLong = {
    val a0 = x.l.toDouble
    val a1 = x.m.toDouble
    val a2 = x.h.toDouble

    val b0 = y.l.toDouble
    val b1 = y.m.toDouble
    val b2 = y.h.toDouble

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

    p1 = p1 + ((p0 / (1 << BITS)) | 0)
    p2 = p2 + ((p1 / (1 << BITS)) | 0)

    val c0 = (p0 & MASK).asInstanceOf[Int]
    val c1 = (p1 & MASK).asInstanceOf[Int]
    val c2 = (p2 & MASK_2).asInstanceOf[Int]

    new RuntimeLong(c0, c1, c2)
  }

  def /(y: RuntimeLong): RuntimeLong = (x divMod y)(0)
  def %(y: RuntimeLong): RuntimeLong = (x divMod y)(1)

  //override def getClass(): Class[Long] = null

  def toBinaryString: String = {
    val zeros = "0000000000000000000000" // 22 zeros
    @inline def padBinary22(i: Int) = {
      val s = Integer.toBinaryString(i)
      zeros.substring(s.length) + s
    }

    if (h != 0) Integer.toBinaryString(h) + padBinary22(m) + padBinary22(l)
    else if (m != 0) Integer.toBinaryString(m) + padBinary22(l)
    else Integer.toBinaryString(l)
  }

  def toHexString: String = {
    val zeros = "000000" // 6 zeros
    @inline def padHex(i: Int, len: Int) = {
      val s = Integer.toHexString(i)
      zeros.substring(s.length + (6-len)) + s
    }

    val mp = m >> 2
    val lp = l | ((m & 0x3) << BITS)

    if (h != 0) Integer.toHexString(h) + padHex(mp, 5) + padHex(lp, 6)
    else if (mp != 0) Integer.toHexString(mp) + padHex(lp, 6)
    else Integer.toHexString(lp)
  }

  def toOctalString: String = {
    val zeros = "0000000" // 7 zeros
    @inline def padOctal7(i: Int) = {
      val s = Integer.toOctalString(i)
      zeros.substring(s.length) + s
    }

    val lp = l & (MASK >> 1)
    val mp = ((m & (MASK >> 2)) << 1) | (l >> (BITS - 1))
    val hp = (h << 2) | (m >> (BITS - 2))

    if (hp != 0) Integer.toOctalString(hp) + padOctal7(mp) + padOctal7(lp)
    else if (mp != 0) Integer.toOctalString(mp) + padOctal7(lp)
    else Integer.toOctalString(lp)
  }

  // Any API //

  override def toString: String = {
    if (isZero) "0"
    // Check for MinValue, because its not negatable
    else if (isMinValue) "-9223372036854775808"
    else if (isNegative) "-" + (-x).toString
    else {
      val tenPow9 = TenPow9 // local copy to access CachedConstants only once

      @tailrec
      @inline
      def toString0(v: RuntimeLong, acc: String): String =
        if (v.isZero) acc
        else {
          val quotRem = v.divMod(tenPow9)
          val quot = quotRem(0)
          val rem = quotRem(1)

          val digits = rem.toInt.toString
          val zeroPrefix =
            if (quot.isZero) ""
            else "000000000".substring(digits.length) // (9 - digits.length) zeros

          toString0(quot, zeroPrefix + digits + acc)
        }

      toString0(x, "")
    }
  }

  def bitCount: Int =
    Integer.bitCount(l) + Integer.bitCount(m) + Integer.bitCount(h)

  // helpers //

  @inline private def isZero = l == 0 && m == 0 && h == 0
  @inline private def isMinValue = x.equals(MinValue)
  @inline private def isNegative = (h & SIGN_BIT_VALUE) != 0
  @inline private def abs = if (isNegative) -x else x

  def signum: RuntimeLong =
    if (isNegative) MinusOne else if (isZero) Zero else One

  def numberOfLeadingZeros: Int = {
    if (h != 0)      Integer.numberOfLeadingZeros(h) - (32 - BITS2)
    else if (m != 0) Integer.numberOfLeadingZeros(m) - (32 - BITS) + (64 - BITS01)
    else             Integer.numberOfLeadingZeros(l) - (32 - BITS) + (64 - BITS)
  }

  def numberOfTrailingZeros: Int = {
    if (l != 0)      Integer.numberOfTrailingZeros(l)
    else if (m != 0) Integer.numberOfTrailingZeros(m) + BITS
    else             Integer.numberOfTrailingZeros(h) + BITS01
  }

  /** return log_2(x) if power of 2 or -1 otherwise */
  private def powerOfTwo = {
    if (h == 0 && m == 0 && l != 0 && (l & (l - 1)) == 0)
      Integer.numberOfTrailingZeros(l)
    else if (h == 0 && m != 0 && l == 0 && (m & (m - 1)) == 0)
      Integer.numberOfTrailingZeros(m) + BITS
    else if (h != 0 && m == 0 && l == 0 && (h & (h - 1)) == 0)
      Integer.numberOfTrailingZeros(h) + BITS01
    else
      -1
  }

  private def setBit(bit: Int) =
    if (bit < BITS)
      new RuntimeLong(l | (1 << bit), m, h)
    else if (bit < BITS01)
      new RuntimeLong(l, m | (1 << (bit - BITS)), h)
    else
      new RuntimeLong(l, m, h | (1 << (bit - BITS01)))

  private def divMod(y: RuntimeLong): scala.scalajs.js.Array[RuntimeLong] = {
    // scalastyle:off return
    import scala.scalajs.js
    if (y.isZero) throw new ArithmeticException("/ by zero")
    else if (x.isZero) js.Array(Zero, Zero)
    else if (y.isMinValue) {
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
          if (xMinValue)
            MaxValue
          else {
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
  private def maskRight(bits: Int) = {
    if (bits <= BITS)
      new RuntimeLong(l & ((1 << bits) - 1), 0, 0)
    else if (bits <= BITS01)
      new RuntimeLong(l, m & ((1 << (bits - BITS)) - 1), 0)
    else
      new RuntimeLong(l, m, h & ((1 << (bits - BITS01)) - 1))
  }

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

  /** number of relevant bits in each Long.l and Long.m */
  private final val BITS   = 22
  /** number of relevant bits in Long.l and Long.m together */
  private final val BITS01 = 2 * BITS
  /** number of relevant bits in Long.h */
  private final val BITS2  = 64 - BITS01
  /** bitmask for Long.l and Long.m */
  private final val MASK   = (1 << BITS) - 1
  /** bitmask for Long.h */
  private final val MASK_2 = (1 << BITS2) - 1

  private final val SIGN_BIT       = BITS2 - 1
  private final val SIGN_BIT_VALUE = 1 << SIGN_BIT
  private final val TWO_PWR_15_DBL = 0x8000   * 1.0
  private final val TWO_PWR_16_DBL = 0x10000  * 1.0
  private final val TWO_PWR_22_DBL = 0x400000 * 1.0
  private final val TWO_PWR_31_DBL = TWO_PWR_16_DBL * TWO_PWR_15_DBL
  private final val TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL
  private final val TWO_PWR_44_DBL = TWO_PWR_22_DBL * TWO_PWR_22_DBL
  private final val TWO_PWR_63_DBL = TWO_PWR_32_DBL * TWO_PWR_31_DBL

  // scalastyle:off disallow.space.after.token
  // Cache the instances for some "literals" used in this implementation
  val Zero     = new RuntimeLong(      0,       0,      0) // 0L
  val One      = new RuntimeLong(      1,       0,      0) // 1L
  val MinusOne = new RuntimeLong(   MASK,    MASK, MASK_2) // -1L
  val MinValue = new RuntimeLong(      0,       0, 524288) // Long.MinValue
  val MaxValue = new RuntimeLong(4194303, 4194303, 524287) // Long.MaxValue
  val TenPow9  = new RuntimeLong(1755648,     238,      0) // 1000000000L with 9 zeros
  // scalastyle:on disallow.space.after.token

  def fromDouble(value: Double): RuntimeLong = {
    if (java.lang.Double.isNaN(value)) Zero
    else if (value < -TWO_PWR_63_DBL) MinValue
    else if (value >= TWO_PWR_63_DBL) MaxValue
    else if (value < 0) -fromDouble(-value)
    else {
      var acc = value
      val a2 = if (acc >= TWO_PWR_44_DBL) (acc / TWO_PWR_44_DBL).toInt else 0
      acc -= a2 * TWO_PWR_44_DBL
      val a1 = if (acc >= TWO_PWR_22_DBL) (acc / TWO_PWR_22_DBL).toInt else 0
      acc -= a1 * TWO_PWR_22_DBL
      val a0 = acc.toInt
      new RuntimeLong(a0, a1, a2)
    }
  }

}
