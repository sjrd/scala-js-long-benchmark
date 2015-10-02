package longbenchmark

import scala.scalajs.js
import org.scalajs.dom

object LongBenchmark extends js.JSApp {
  final val SampleCount = 1000

  val longs1 = Array.fill(SampleCount)(scala.util.Random.nextLong())
  val longs2 = Array.fill(SampleCount)(scala.util.Random.nextLong())

  val results = new Array[Long](SampleCount)

  def main(): Unit = {
    val suite = new benchmarkjs.Suite

    val long1 = longs1(0)
    val long2 = longs2(0)

    suite
      .add("+", { () =>
        long1 + long2
      })
      .add("-", { () =>
        long1 - long2
      })
      .add("*", { () =>
        long1 * long2
      })
      .add("/", { () =>
        long1 / long2
      })
      .on("cycle", { (event: js.Dynamic) =>
        println(event.target.toString())
      })
      .run(benchmarkjs.Suite.RunOptions())
  }
}
