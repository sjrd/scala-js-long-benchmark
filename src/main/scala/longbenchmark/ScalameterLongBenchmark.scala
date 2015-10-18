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
  
    val gwtLong = Gen.single("GoogleWebkit")(gwtlike.Build.fromLong _)
    val optgwtLong = Gen.single("OptGoogleWebkit")(optgwtlike.Build.fromLong _)
    val teavmLong = Gen.single("TeaVM")(teavmlike.Build.fromLong _)
    val optteavmLong = Gen.single("OptTeaVM")(optteavmlike.Build.fromLong _)
    
    performance of "RuntimeLong" in {
      
       val numbers = for (
            i <- 1L to 1000L;
            j <- (Long.MaxValue - 1000) to Long.MaxValue) yield (i, j)
      
      measure method "+" in {
            
        using(gwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) + longBuilder(b)
          }
        }
        
        using(optgwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) + longBuilder(b)
          }
        }
        
        using(teavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) + longBuilder(b)
          }
        }
        
        using(optteavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) + longBuilder(b)
          }
        }
      }
       
       measure method "-" in {
            
        using(gwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) - longBuilder(b)
          }
        }
        
        using(optgwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) - longBuilder(b)
          }
        }
        
        using(teavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) - longBuilder(b)
          }
        }
        
        using(optteavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) - longBuilder(b)
          }
        }
      }
       
       measure method "*" in {
            
        using(gwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) * longBuilder(b)
          }
        }
        
        using(optgwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) * longBuilder(b)
          }
        }
        
        using(teavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) * longBuilder(b)
          }
        }
        
        using(optteavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) * longBuilder(b)
          }
        }
      }
       
       measure method "/" in {
            
        using(gwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) / longBuilder(b)
          }
        }
        
        using(optgwtLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) / longBuilder(b)
          }
        }
        
        /*using(teavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) / longBuilder(b)
          }
        }*/
        
        using(optteavmLong) in { longBuilder =>
          numbers.foreach {
            case (a, b) => longBuilder(a) / longBuilder(b)
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
      }

      def report(results: Tree[CurveData[Double]], persistor: Persistor): Boolean = {
        println("\n\nAll Test finished : ")

        true
      }

    }

    override def executor = LocalExecutor(warmer, aggregator, measurer)
    override def persistor = Persistor.None
  }
  
  
  
  def main : Unit = {
     LongBench.main(Array())
   } 
}