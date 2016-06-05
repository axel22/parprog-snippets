package lectures
package dataparallelism

import scala.collection._
import org.scalameter._

object Agnostic {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  val array = (0 until 1000000).toArray
  
  def largestPalindrome(xs: GenSeq[Int]): Int = {
    xs.aggregate(0)(
      (largest, n) => if (n > largest && n.toString == n.toString.reverse) n else largest,
      math.max
    )
  }

  def main(args: Array[String]) {
    val seqtime = standardConfig measure {
      largestPalindrome(array)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      largestPalindrome(array.par)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
  
}
