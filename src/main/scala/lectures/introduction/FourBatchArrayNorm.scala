package lectures
package introduction

import org.scalameter._
import common._

object FourBatchArrayNorm {
  @volatile var dummy: Int = 0
  @volatile var dummy2: Int = 0

  val logE = math.log(math.E)

  def power(x: Int, p: Double): Int = {
    math.exp(p * math.log(x) / logE).toInt // TODO <-- make everything doubles
  }

  def sumSegment(xs: Array[Int], p: Double, from: Int, until: Int): Int = {
    var i = from
    var s = 0
    while (i < until) {
      s += power(xs(i), p)
      i += 1
    }
    s
  }

  def normSum(xs: Array[Int], p: Double): Int =
    power(sumSegment(xs, p, 0, xs.size), 1.0 / p)

  def fjNormSum(xs: Array[Int], p: Double, threshold: Int): Int = {
    val ((s1, s2), (s3, s4)) = parallel(
      parallel(
        sumSegment(xs, p, 0, xs.length / 4),
        sumSegment(xs, p, xs.length / 4, 2 * xs.length / 4)
      ),
      parallel(
        sumSegment(xs, p, 2 * xs.length / 4, 3 * xs.length / 4),
        sumSegment(xs, p, 3 * xs.length / 4, xs.length)
      )
    )
    power(s1 + s2 + s3 + s4, 1.0 / p)
  }

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val p = 1.5
    val xs = (0 until 2000000).map(_ % 100).toArray
    val seqtime = standardConfig measure {
      dummy = normSum(xs, p)
    }
    println(s"sequential sum time: $seqtime ms")

    val threshold = 10000
    val fjtime = standardConfig measure {
      dummy2 = fjNormSum(xs, p, threshold)
    }
    println(s"values computed are $dummy vs $dummy2")
    println(s"fork/join time: $fjtime ms")
    println(s"speedup: ${seqtime/fjtime}")
  }

}
