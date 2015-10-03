package longbenchmark.gwtlike

object Build {
  @inline
  def fromLong(value: Long): RuntimeLong = {
    new RuntimeLong(
        value.toInt & ((1 << 22) - 1),
        (value >>> 22).toInt & ((1 << 22) - 1),
        (value >>> 44).toInt)
  }

  @inline
  def fromInt(value: Int): RuntimeLong =
    new RuntimeLong(value)
}
