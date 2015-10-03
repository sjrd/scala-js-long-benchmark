package longbenchmark.optteavmlike

import scala.scalajs.js

object Build {
  private[optteavmlike] final val MaxNormal = 1 << 18
  private[optteavmlike] final val TwoPow32 = 4294967296.0

  @inline
  def fromLong(value: Long): RuntimeLong = {
    new RuntimeLong(value.toInt, (value >>> 32).toInt)
  }

  @inline
  def fromInt(value: Int): RuntimeLong =
    new RuntimeLong(value, value >> 31)

  @inline
  def fromPair(pair: (Int, Int)): RuntimeLong =
    new RuntimeLong(pair._1, pair._2)

  @inline
  private[optteavmlike] def rawToInt(v: Double): Int =
    (v.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
}
