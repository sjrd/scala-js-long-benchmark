package longbenchmark

import scala.scalajs.js
import js.annotation._
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

    val sample: Gen[Seq[(Long, Long)]] = Gen.unit("sample1").map {
      _ =>
        for (
          i <- 1L to 1000L;
          j <- (Long.MaxValue - 1000) to Long.MaxValue
        ) yield (i, j)
    }

    // Google Web Toolkit
    performance of "gwt" in {
      measure method "plus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              gwtlike.Build.fromLong(a) + gwtlike.Build.fromLong(b)
          }
        }
      }

      measure method "minus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              gwtlike.Build.fromLong(a) - gwtlike.Build.fromLong(b)
          }
        }
      }
      
      measure method "times" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              gwtlike.Build.fromLong(a) * gwtlike.Build.fromLong(b)
          }
        }
      }
      
      measure method "div" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              gwtlike.Build.fromLong(a) / gwtlike.Build.fromLong(b)
          }
        }
      }
    }
    
    // Optimized Google Web Toolkit
    performance of "optgwt" in {
      measure method "plus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optgwtlike.Build.fromLong(a) + optgwtlike.Build.fromLong(b)
          }
        }
      }

      measure method "minus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optgwtlike.Build.fromLong(a) - optgwtlike.Build.fromLong(b)
          }
        }
      }
      
      measure method "times" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optgwtlike.Build.fromLong(a) * optgwtlike.Build.fromLong(b)
          }
        }
      }
      
      measure method "div" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optgwtlike.Build.fromLong(a) / optgwtlike.Build.fromLong(b)
          }
        }
      }
    }
    
    // Tea VM
    performance of "teavm" in {
      measure method "plus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              teavmlike.Build.fromLong(a) + teavmlike.Build.fromLong(b)
          }
        }
      }

      measure method "minus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              teavmlike.Build.fromLong(a) - teavmlike.Build.fromLong(b)
          }
        }
      }
      
      measure method "times" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              teavmlike.Build.fromLong(a) * teavmlike.Build.fromLong(b)
          }
        }
      }
      
      /*measure method "div" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              teavm.Build.fromLong(a) / teavm.Build.fromLong(b)
          }
        }
      }*/
    }
    
    // Optimized Tea VM
    performance of "optteavm" in {
      measure method "plus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optteavmlike.Build.fromLong(a) + optteavmlike.Build.fromLong(b)
          }
        }
      }

      measure method "minus" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optteavmlike.Build.fromLong(a) - optteavmlike.Build.fromLong(b)
          }
        }
      }
      
      measure method "times" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optteavmlike.Build.fromLong(a) * optteavmlike.Build.fromLong(b)
          }
        }
      }
      
      measure method "div" in {
        using(sample) in { numbers =>
          numbers.foreach {
            case (a, b) =>
              optteavmlike.Build.fromLong(a) / optteavmlike.Build.fromLong(b)
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