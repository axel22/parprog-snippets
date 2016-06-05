package lectures
package reductions

import org.scalameter._
import common._

object ArraySum {

  @volatile var dummy: Int = 0

  def sum(xs: Array[Int], from: Int, until: Int): Int = {
    var i = from
    var s = 0
    while (i < until) {
      s += xs(i)
      i += 1
    }
    s
  }

  def parSum(xs: Array[Int], from: Int, until: Int, threshold: Int): Int = {
    if (until - from < threshold) {
      sum(xs, from, until)
    } else {
      val mid = (from + until) / 2
      val right = task {
        parSum(xs, mid, until, threshold)
      }
      val leftSum = parSum(xs, from, mid, threshold)
      val rightSum = right.join()
      leftSum + rightSum
    }
  }

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val xs = (0 until 100000000).map(_ % 100).toArray
    val seqtime = standardConfig measure {
      dummy = sum(xs, 0, xs.length)
    }
    println(s"sequential sum time: $seqtime ms")

    val fjtime = standardConfig measure {
      parSum(xs, 0, xs.length, 10000)
    }
    println(s"fork/join time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }

}
