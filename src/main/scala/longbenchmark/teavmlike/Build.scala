package longbenchmark.teavmlike

import scala.scalajs.js

object Build {
  private[teavmlike] final val MaxNormal = 1 << 18
  private[teavmlike] final val TwoPow32 = 4294967296.0

  @inline
  def fromLong(value: Long): RuntimeLong = {
    new RuntimeLong(value.toInt, (value >>> 32).toInt)
  }

  @inline
  def fromInt(value: Int): RuntimeLong =
    new RuntimeLong(value, value >> 31)

  @inline
  private[teavmlike] def rawToInt(v: Double): Int =
    (v.asInstanceOf[js.Dynamic] | 0.asInstanceOf[js.Dynamic]).asInstanceOf[Int]
}
