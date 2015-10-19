package longbenchmark

import scala.scalajs.js
import js.annotation._
import js.JSConverters._
import org.scalajs.dom
import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import org.scalameter.utils.Tree
import org.scalameter.CurveData

/**
 * @author dengels
 */
object ScalameterLongBenchmark extends js.JSApp {

  object LongBench extends Bench.OfflineReport {

    //val gwtLong = Gen.single("gwt")(gwtlike.Build.fromLong _)
    //val optgwtLong = Gen.single("optgwt")(optgwtlike.Build.fromLong _)
    //val teavmLong = Gen.single("teavm")(teavmlike.Build.fromLong _)
    //val optteavmLong = Gen.single("optteavm")(optteavmlike.Build.fromLong _)

    final class GWTPair(val a: gwtlike.RuntimeLong, val b: gwtlike.RuntimeLong)
    final class OptGWTPair(val a: optgwtlike.RuntimeLong, val b: optgwtlike.RuntimeLong)
    final class TeaVMPair(val a: teavmlike.RuntimeLong, val b: teavmlike.RuntimeLong)
    final class OptTeaVMPair(val a: optteavmlike.RuntimeLong, val b: optteavmlike.RuntimeLong)

    val longSample = {
      (for {
        i <- 1L to 1000L
        j <- (Long.MaxValue - 1000) to Long.MaxValue
      } yield (j, i)).toJSArray
    }

    val gwtSample = Gen.unit("gwt").map(_ => longSample.map {
      case (i, j) => new GWTPair(gwtlike.Build.fromLong(i), gwtlike.Build.fromLong(j))
    }).cached

    val optgwtSample = Gen.unit("optgwt").map(_ => longSample.map {
      case (i, j) => new OptGWTPair(optgwtlike.Build.fromLong(i), optgwtlike.Build.fromLong(j))
    }).cached

    val teavmSample = Gen.unit("teavm").map(_ => longSample.map {
      case (i, j) => new TeaVMPair(teavmlike.Build.fromLong(i), teavmlike.Build.fromLong(j))
    }).cached

    val optteavmSample = Gen.unit("optteavm").map(_ => longSample.map {
      case (i, j) => new OptTeaVMPair(optteavmlike.Build.fromLong(i), optteavmlike.Build.fromLong(j))
    }).cached

    // Google Web Toolkit
    performance of "gwt" in {
      measure method "plus" in {
        using(gwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a + pair.b
          }
        }
      }

      measure method "minus" in {
        using(gwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a - pair.b
          }
        }
      }

      measure method "times" in {
        using(gwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a * pair.b
          }
        }
      }

      measure method "div" in {
        using(gwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a / pair.b
          }
        }
      }
    }

    // Optimized Google Web Toolkit
    performance of "optgwt" in {
      measure method "plus" in {
        using(optgwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a + pair.b
          }
        }
      }

      measure method "minus" in {
        using(optgwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a - pair.b
          }
        }
      }

      measure method "times" in {
        using(optgwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a * pair.b
          }
        }
      }

      measure method "div" in {
        using(optgwtSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a / pair.b
          }
        }
      }
    }

    // Tea VM
    performance of "teavm" in {
      measure method "plus" in {
        using(teavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a + pair.b
          }
        }
      }

      measure method "minus" in {
        using(teavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a - pair.b
          }
        }
      }

      measure method "times" in {
        using(teavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a * pair.b
          }
        }
      }

      /*measure method "div" in {
        using(teavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a / pair.b
          }
        }
      }*/
    }

    // Optimized Tea VM
    performance of "optteavm" in {
      measure method "plus" in {
        using(optteavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a + pair.b
          }
        }
      }

      measure method "minus" in {
        using(optteavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a - pair.b
          }
        }
      }

      measure method "times" in {
        using(optteavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a * pair.b
          }
        }
      }

      measure method "div" in {
        using(optteavmSample) in { numbers =>
          numbers.foreach { pair =>
            pair.a / pair.b
          }
        }
      }
    }

    override def reporter = new Reporter[Double] {
      def report(result: CurveData[Double], persistor: Persistor): Unit = {
        println(s"\nOne Test finished : success = ${result.success}")

        println("\nContext:")
        val ctx = result.context
        ctx.properties.foreach { x => println(s"${x._1} -> ${x._2}") }

        println("\nInfo:")
        val info = result.info
        info.foreach { x => println(s"${x._1} -> ${x._2}") }

        println("\nMeasurements:")
        val measurements = result.measurements
        measurements.foreach { x =>
          val axisData = x.params.toString
          val result = x.value + x.units
          println(s"$axisData : $result")
        }

        val measure = math.round(measurements.head.value * 100) / 100
        val units = measurements.head.units

        val id = ctx.scopeList.mkString("-")
        println(s"id = $id")
        dom.document.getElementById(id).innerHTML = s"$measure $units"
        js.Dynamic.global.$.apply(dom.window).trigger("resize")
        dom.document.getElementById("results").asInstanceOf[js.Dynamic].style.display = "none";
        println(dom.document.getElementById("results").asInstanceOf[js.Dynamic].offsetWidth)
        dom.document.getElementById("results").asInstanceOf[js.Dynamic].style.display = "block";
        println(dom.document.getElementById("results").asInstanceOf[js.Dynamic].offsetWidth)
      }

      def report(results: Tree[CurveData[Double]], persistor: Persistor): Boolean = {
        println("\n\nAll Test finished : ")

        true
      }

    }

    override def executor = LocalExecutor(warmer, aggregator, measurer)
    override def persistor = Persistor.None
  }

  def main: Unit = {
    LongBench.main(Array())
  }
}
