package benchmarkjs

import scala.scalajs.js
import js.annotation._

@js.native
@JSName("Benchmark.Suite")
class Suite(val name: String = ???) extends js.Object {
  def add(name: String, fn: js.Function0[_]): this.type = js.native

  def on(tpe: String, listener: js.Function0[_]): this.type = js.native
  def on(tpe: String, listener: js.Function1[js.Dynamic, _]): this.type = js.native

  def run(options: Suite.RunOptions = ???): Unit = js.native
}

object Suite {
  @ScalaJSDefined
  trait RunOptions extends js.Object {
    def async: Boolean
    def queued: Boolean
  }

  object RunOptions {
    def apply(async: Boolean = true, queued: Boolean = true): RunOptions = {
      js.Dynamic.literal(
          async = async,
          queued = queued
      ).asInstanceOf[RunOptions]
    }
  }
}
