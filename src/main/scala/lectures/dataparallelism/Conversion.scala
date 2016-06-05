package lectures
package dataparallelism

import scala.collection._
import org.scalameter._

object Conversion {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 20,
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  val array = Array.fill(10000000)("")
  val list = array.toList

  def main(args: Array[String]) {
    val listtime = standardConfig measure {
      list.par
    }
    println(s"list conversion time: $listtime ms")

    val arraytime = standardConfig measure {
      array.par
    }
    println(s"array conversion time: $arraytime ms")
    println(s"difference: ${listtime / arraytime}")
  }
  
}
