package lectures
package examples

import org.scalameter._
import common._

/** An example of a trivially parallelizable brute force solution.
 *  The nice thing about this example is that computing the collatz sequence
 *  does not require any memory access, so the memory bandwidth is not a bottleneck.
 *  Here we can really see the benefits of a quad-core with hyperthreading.
 */
object BruteForceCollatzSequence {

  @volatile var dummy = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 30,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def collatz(number: Int): Int = {
    var length = 1
    var n = number
    while (n != 1) {
      if (n % 2 == 0) n = n / 2
      else n = 3 * n + 1
      length += 1
    }
    length
  }

  def longestCollatz(from: Int, until: Int): Int = {
    (from until until).maxBy(collatz)
  }

  def fjLongestCollatz(from: Int, until: Int, threshold: Int): Int = {
    if (until - from < threshold) {
      longestCollatz(from, until)
    } else {
      val mid = (from + until) / 2
      val (leftLongest,rightLongest) =
	parallel(fjLongestCollatz(from, mid, threshold),
		 fjLongestCollatz(mid, until, threshold))
      math.max(leftLongest, rightLongest)
    }
  }

  def main(args: Array[String]) {
    val until = 100000
    val threshold = 100
    val seqtime = standardConfig measure {
      dummy = longestCollatz(1, until)
    }
    println(s"sequential sum time: $seqtime ms")

    val fjtime = standardConfig measure {
      fjLongestCollatz(1, until, threshold)
    }
    println(s"fork/join time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }

}
