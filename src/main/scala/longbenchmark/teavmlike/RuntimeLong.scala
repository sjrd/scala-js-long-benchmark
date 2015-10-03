package longbenchmark.teavmlike

import scala.scalajs.js
import js.JSNumberOps._

class RuntimeLong(val lo: Int, val hi: Int) { a =>
  import Build._

  @inline private def isInt: Boolean = hi == (lo >> 31)

  def notEquals(b: RuntimeLong): Boolean =
    a.lo != b.lo || a.hi != b.hi

  override def toString(): String = {
    "(" + lo + ", " + hi + ")"
  }

  @inline
  def fromNumber(v: Double): RuntimeLong = {
    new RuntimeLong(rawToInt(v),
        if (v >= 0.0) rawToInt(v / TwoPow32)
        else rawToInt((v / TwoPow32) - 1.0))
  }

  @inline
  def toNumber: Double = {
    lo.toUint + (hi * TwoPow32)
  }

  @inline
  private def isNegative: Boolean = hi < 0

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

  @noinline
  def unary_- : RuntimeLong =
    this.inline_~.inline_inc

  def +(b: RuntimeLong): RuntimeLong = {
    if (a.isInt && b.isInt) {
      fromNumber(a.lo.toDouble + b.lo.toDouble)
    } else if (Math.abs(a.hi) < MaxNormal && Math.abs(b.hi) < MaxNormal) {
      fromNumber(a.toNumber + b.toNumber)
    } else {
      def slowPath(): RuntimeLong = {
        val a_lolo = a.lo & 0xFFFF
        val a_lohi = a.lo >>> 16
        val a_hilo = a.hi & 0xFFFF
        val a_hihi = a.hi >>> 16
        val b_lolo = b.lo & 0xFFFF
        val b_lohi = b.lo >>> 16
        val b_hilo = b.hi & 0xFFFF
        val b_hihi = b.hi >>> 16

        val lolo = (a_lolo + b_lolo) | 0
        val lohi = (a_lohi + b_lohi + (lolo >> 16)) | 0
        val hilo = (a_hilo + b_hilo + (lohi >> 16)) | 0
        val hihi = (a_hihi + b_hihi + (hilo >> 16)) | 0

        new RuntimeLong(
            (lolo & 0xFFFF) | ((lohi & 0xFFFF) << 16),
            (hilo & 0xFFFF) | ((hihi & 0xFFFF) << 16))
      }

      slowPath()
    }
  }

  def -(b: RuntimeLong): RuntimeLong = {
    if (a.isInt && b.isInt) {
      fromNumber(a.lo.toDouble - b.lo.toDouble)
    } else {
      def slowPath(): RuntimeLong = {
        val a_lolo = a.lo & 0xFFFF
        val a_lohi = a.lo >>> 16
        val a_hilo = a.hi & 0xFFFF
        val a_hihi = a.hi >>> 16
        val b_lolo = b.lo & 0xFFFF
        val b_lohi = b.lo >>> 16
        val b_hilo = b.hi & 0xFFFF
        val b_hihi = b.hi >>> 16

        val lolo = (a_lolo - b_lolo) | 0
        val lohi = (a_lohi - b_lohi + (lolo >> 16)) | 0
        val hilo = (a_hilo - b_hilo + (lohi >> 16)) | 0
        val hihi = (a_hihi - b_hihi + (hilo >> 16)) | 0

        new RuntimeLong(
            (lolo & 0xFFFF) | ((lohi & 0xFFFF) << 16),
            (hilo & 0xFFFF) | ((hihi & 0xFFFF) << 16))
      }

      slowPath()
    }
  }

  def *(b0: RuntimeLong): RuntimeLong = {
    val positive = this.isNegative == b0.isNegative
    val a =
      if (this.isNegative) -this
      else this
    val b =
      if (b0.isNegative) -b0
      else b0

    val a_lolo = a.lo & 0xFFFF
    val a_lohi = a.lo >>> 16
    val a_hilo = a.hi & 0xFFFF
    val a_hihi = a.hi >>> 16
    val b_lolo = b.lo & 0xFFFF
    val b_lohi = b.lo >>> 16
    val b_hilo = b.hi & 0xFFFF
    val b_hihi = b.hi >>> 16

    var lolo = 0
    var lohi = 0
    var hilo = 0
    var hihi = 0
    lolo = a_lolo * b_lolo
    lohi = lolo >>> 16
    lohi = (lohi & 0xFFFF) + a_lohi * b_lolo
    hilo = hilo + (lohi >>> 16)
    lohi = (lohi & 0xFFFF) + a_lolo * b_lohi
    hilo = hilo + (lohi >>> 16)
    hihi = hilo >>> 16
    hilo = (hilo & 0xFFFF) + a_hilo * b_lolo
    hihi = hihi + (hilo >>> 16)
    hilo = (hilo & 0xFFFF) + a_lohi * b_lohi
    hihi = hihi + (hilo >>> 16)
    hilo = (hilo & 0xFFFF) + a_lolo * b_hilo
    hihi = hihi + (hilo >>> 16)
    hihi = hihi + a_hihi * b_lolo + a_hilo * b_lohi + a_lohi * b_hilo + a_lolo * b_hihi

    val result = new RuntimeLong(
        (lolo & 0xFFFF) | (lohi << 16),
        (hilo & 0xFFFF) | (hihi << 16))
    if (positive) result
    else -result
  }
}
