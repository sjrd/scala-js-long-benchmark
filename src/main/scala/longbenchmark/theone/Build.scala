package longbenchmark.theone

import scala.scalajs.js

object Build {
  private[theone] final val MaxNormal = 1 << 18
  private[theone] final val TwoPow32 = 4294967296.0

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
  private[theone] def rawToInt(v: Double): Int =
    (v.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
}
