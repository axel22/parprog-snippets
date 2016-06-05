package lectures
package dataparallelism

import org.scalameter._

object ArrayInitialization {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  val array = new Array[Int](100000000)
  
  def main(args: Array[String]) {
    val value = 100
    val seqtime = standardConfig measure {
      for (i <- 0 until array.length) {
        array(i) = value
      }
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      for (i <- (0 until array.length).par) {
        array(i) = value
      }
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
  
}
