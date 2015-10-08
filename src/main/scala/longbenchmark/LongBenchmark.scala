package longbenchmark

import scala.scalajs.js
import js.annotation._
import org.scalajs.dom

object LongBenchmark extends js.JSApp {
  val isBrowser = !js.isUndefined(js.Dynamic.global.window)

  final val SampleCount = 1000

  val longs1 = Array.fill(SampleCount)(scala.util.Random.nextLong())
  val longs2 = Array.fill(SampleCount)(scala.util.Random.nextLong())

  val results = new Array[Long](SampleCount)

  def main(): Unit = {
    for (i <- 0 until SampleCount) {
      val l = longs1(i)
      val r = longs2(i)
      //val r = scala.util.Random.nextInt().toLong
      val expected = optteavmlike.Build.fromLong(l % r)
      val actual = optteavmlike.Build.fromLong(l) % optteavmlike.Build.fromLong(r)
      if (expected.notEquals(actual))
        println(s"$l / $r  expected $expected got $actual")
    }

    doAll()
    doAll()
  }

  def doAll(): Unit = {
    unaryMinus()
    plusIntValues()
    minusIntValues()
    timesIntValues()
    divideIntValues()
    divideDoubleValues()
    divideLongValues()
    remainderIntValues()
    remainderDoubleValues()
    remainderLongValues()
  }

  def unaryMinus() {
    val suite = new benchmarkjs.Suite("-value")

    val gwtlike1 = gwtlike.Build.fromInt(1234567892)
    val gwtlike2 = gwtlike.Build.fromInt(987654321)

    val optgwtlike1 = optgwtlike.Build.fromInt(1234567892)
    val optgwtlike2 = optgwtlike.Build.fromInt(987654321)

    val teavmlike1 = teavmlike.Build.fromInt(1234567892)
    val teavmlike2 = teavmlike.Build.fromInt(987654321)

    val optteavmlike1 = optteavmlike.Build.fromInt(1234567892)
    val optteavmlike2 = optteavmlike.Build.fromInt(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        js.Array(-gwtlike1, -gwtlike2)
      })
      .add("Opt GWT-like", { () =>
        js.Array(-optgwtlike1, -optgwtlike2)
      })
      .add("TeaVM-like", { () =>
        js.Array(-teavmlike1, -teavmlike2)
      })
      .add("Opt TeaVM-like", { () =>
        js.Array(-optteavmlike1, -optteavmlike2)
      }))
  }

  def plusIntValues() {
    val suite = new benchmarkjs.Suite("int + int")

    val gwtlike1 = gwtlike.Build.fromInt(1234567892)
    val gwtlike2 = gwtlike.Build.fromInt(987654321)

    val optgwtlike1 = optgwtlike.Build.fromInt(1234567892)
    val optgwtlike2 = optgwtlike.Build.fromInt(987654321)

    val teavmlike1 = teavmlike.Build.fromInt(1234567892)
    val teavmlike2 = teavmlike.Build.fromInt(987654321)

    val optteavmlike1 = optteavmlike.Build.fromInt(1234567892)
    val optteavmlike2 = optteavmlike.Build.fromInt(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 + gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 + optgwtlike2
      })
      .add("TeaVM-like", { () =>
        teavmlike1 + teavmlike2
      })
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 + optteavmlike2
      }))
  }

  def minusIntValues() {
    val suite = new benchmarkjs.Suite("int - int")

    val gwtlike1 = gwtlike.Build.fromInt(1234567892)
    val gwtlike2 = gwtlike.Build.fromInt(987654321)

    val optgwtlike1 = optgwtlike.Build.fromInt(1234567892)
    val optgwtlike2 = optgwtlike.Build.fromInt(987654321)

    val teavmlike1 = teavmlike.Build.fromInt(1234567892)
    val teavmlike2 = teavmlike.Build.fromInt(987654321)

    val optteavmlike1 = optteavmlike.Build.fromInt(1234567892)
    val optteavmlike2 = optteavmlike.Build.fromInt(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 - gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 - optgwtlike2
      })
      .add("TeaVM-like", { () =>
        teavmlike1 - teavmlike2
      })
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 - optteavmlike2
      }))
  }

  def timesIntValues() {
    val suite = new benchmarkjs.Suite("int * int")

    val gwtlike1 = gwtlike.Build.fromInt(1234567892)
    val gwtlike2 = gwtlike.Build.fromInt(987654321)

    val optgwtlike1 = optgwtlike.Build.fromInt(1234567892)
    val optgwtlike2 = optgwtlike.Build.fromInt(987654321)

    val teavmlike1 = teavmlike.Build.fromInt(1234567892)
    val teavmlike2 = teavmlike.Build.fromInt(987654321)

    val optteavmlike1 = optteavmlike.Build.fromInt(1234567892)
    val optteavmlike2 = optteavmlike.Build.fromInt(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 * gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 * optgwtlike2
      })
      .add("TeaVM-like", { () =>
        teavmlike1 * teavmlike2
      })
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 * optteavmlike2
      }))
  }

  def divideIntValues() {
    val suite = new benchmarkjs.Suite("int / int")

    val gwtlike1 = gwtlike.Build.fromInt(1234567892)
    val gwtlike2 = gwtlike.Build.fromInt(987654321)

    val optgwtlike1 = optgwtlike.Build.fromInt(1234567892)
    val optgwtlike2 = optgwtlike.Build.fromInt(987654321)

    val teavmlike1 = teavmlike.Build.fromInt(1234567892)
    val teavmlike2 = teavmlike.Build.fromInt(987654321)

    val optteavmlike1 = optteavmlike.Build.fromInt(1234567892)
    val optteavmlike2 = optteavmlike.Build.fromInt(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 / gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 / optgwtlike2
      })
      /*.add("TeaVM-like", { () =>
        teavmlike1 / teavmlike2
      })*/
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 / optteavmlike2
      }))
  }

  def divideDoubleValues() {
    val suite = new benchmarkjs.Suite("double / double")

    val gwtlike1 = gwtlike.Build.fromLong(-5620208150665465L)
    val gwtlike2 = gwtlike.Build.fromLong(987654321)

    val optgwtlike1 = optgwtlike.Build.fromLong(-5620208150665465L)
    val optgwtlike2 = optgwtlike.Build.fromLong(987654321)

    val teavmlike1 = teavmlike.Build.fromLong(-5620208150665465L)
    val teavmlike2 = teavmlike.Build.fromLong(987654321)

    val optteavmlike1 = optteavmlike.Build.fromLong(-5620208150665465L)
    val optteavmlike2 = optteavmlike.Build.fromLong(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 / gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 / optgwtlike2
      })
      /*.add("TeaVM-like", { () =>
        teavmlike1 / teavmlike2
      })*/
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 / optteavmlike2
      }))
  }

  def divideLongValues() {
    val suite = new benchmarkjs.Suite("long / long")

    val gwtlike1 = gwtlike.Build.fromLong(-5620208150665465747L)
    val gwtlike2 = gwtlike.Build.fromLong(987654321)

    val optgwtlike1 = optgwtlike.Build.fromLong(-5620208150665465747L)
    val optgwtlike2 = optgwtlike.Build.fromLong(987654321)

    val teavmlike1 = teavmlike.Build.fromLong(-5620208150665465747L)
    val teavmlike2 = teavmlike.Build.fromLong(987654321)

    val optteavmlike1 = optteavmlike.Build.fromLong(-5620208150665465747L)
    val optteavmlike2 = optteavmlike.Build.fromLong(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 / gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 / optgwtlike2
      })
      /*.add("TeaVM-like", { () =>
        teavmlike1 / teavmlike2
      })*/
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 / optteavmlike2
      }))
  }

  def remainderIntValues() {
    val suite = new benchmarkjs.Suite("int % int")

    val gwtlike1 = gwtlike.Build.fromInt(1234567892)
    val gwtlike2 = gwtlike.Build.fromInt(987654321)

    val optgwtlike1 = optgwtlike.Build.fromInt(1234567892)
    val optgwtlike2 = optgwtlike.Build.fromInt(987654321)

    val teavmlike1 = teavmlike.Build.fromInt(1234567892)
    val teavmlike2 = teavmlike.Build.fromInt(987654321)

    val optteavmlike1 = optteavmlike.Build.fromInt(1234567892)
    val optteavmlike2 = optteavmlike.Build.fromInt(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 % gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 % optgwtlike2
      })
      /*.add("TeaVM-like", { () =>
        teavmlike1 % teavmlike2
      })*/
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 % optteavmlike2
      }))
  }

  def remainderDoubleValues() {
    val suite = new benchmarkjs.Suite("double % double")

    val gwtlike1 = gwtlike.Build.fromLong(-5620208150665465L)
    val gwtlike2 = gwtlike.Build.fromLong(987654321)

    val optgwtlike1 = optgwtlike.Build.fromLong(-5620208150665465L)
    val optgwtlike2 = optgwtlike.Build.fromLong(987654321)

    val teavmlike1 = teavmlike.Build.fromLong(-5620208150665465L)
    val teavmlike2 = teavmlike.Build.fromLong(987654321)

    val optteavmlike1 = optteavmlike.Build.fromLong(-5620208150665465L)
    val optteavmlike2 = optteavmlike.Build.fromLong(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 % gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 % optgwtlike2
      })
      /*.add("TeaVM-like", { () =>
        teavmlike1 % teavmlike2
      })*/
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 % optteavmlike2
      }))
  }

  def remainderLongValues() {
    val suite = new benchmarkjs.Suite("long % long")

    val gwtlike1 = gwtlike.Build.fromLong(-5620208150665465747L)
    val gwtlike2 = gwtlike.Build.fromLong(987654321)

    val optgwtlike1 = optgwtlike.Build.fromLong(-5620208150665465747L)
    val optgwtlike2 = optgwtlike.Build.fromLong(987654321)

    val teavmlike1 = teavmlike.Build.fromLong(-5620208150665465747L)
    val teavmlike2 = teavmlike.Build.fromLong(987654321)

    val optteavmlike1 = optteavmlike.Build.fromLong(-5620208150665465747L)
    val optteavmlike2 = optteavmlike.Build.fromLong(987654321)

    runSuite(suite
      .add("GWT-like", { () =>
        gwtlike1 % gwtlike2
      })
      .add("Opt GWT-like", { () =>
        optgwtlike1 % optgwtlike2
      })
      /*.add("TeaVM-like", { () =>
        teavmlike1 % teavmlike2
      })*/
      .add("Opt TeaVM-like", { () =>
        optteavmlike1 % optteavmlike2
      }))
  }

  def runSuite(suite: benchmarkjs.Suite): Unit = {
    suite
      .on("cycle", { (event: js.Dynamic) =>
        println(suite.name + "  " + event.target.toString())
      })
      .run(benchmarkjs.Suite.RunOptions(async = isBrowser, queued = isBrowser))
  }
}
