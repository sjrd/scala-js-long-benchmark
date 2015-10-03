package longbenchmark.optteavmlike

import scala.scalajs.js
import js.JSNumberOps._

class RuntimeLong(val lo: Int, val hi: Int) { a =>
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
}
