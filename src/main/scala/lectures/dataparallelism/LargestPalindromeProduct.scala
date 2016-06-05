package lectures
package dataparallelism

import org.scalameter._

object LargestPalindromeProduct {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)
  
  def main(args: Array[String]) {
    val value = 100
    val seqtime = standardConfig measure {
      (100 to 999).flatMap(i => (i to 999).map(i * _))
        .filter(n => n.toString == n.toString.reverse).max
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      (100 to 999).par.flatMap(i => (i to 999).map(i * _))
        .filter(n => n.toString == n.toString.reverse).max
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
  
}
