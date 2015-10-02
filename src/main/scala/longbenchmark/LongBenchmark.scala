package longbenchmark

import scala.scalajs.js
import org.scalajs.dom

object LongBenchmark extends js.JSApp {
  def main(): Unit = {
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = "<strong>It works!</strong>"
    dom.document.getElementById("playground").appendChild(paragraph)
  }
}
