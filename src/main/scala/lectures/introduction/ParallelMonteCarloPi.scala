package lectures
package introduction

import org.scalameter._
import scala.util.Random
import common._

object ParallelMonteCarloPi {
  @volatile var seqResult: Double = 0
  @volatile var parResult: Double = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def monteCarloPi(iterations: Int): Double = {
    val randomX = new Random
    val randomY = new Random
    var hits = 0
    for (i <- 0 until iterations) {
      val x = randomX.nextDouble()
      val y = randomY.nextDouble()
      if (x * x + y * y < 1) hits += 1
    }
    // r * r * Pi = hitRatio * 4 * r * r
    // Pi = hitRatio * 4
    4.0 * hits / iterations
  }

  def parMonteCarloPi(iterations: Int): Double = {
    val ((pi1, pi2), (pi3, pi4)) = parallel(
      parallel(monteCarloPi(iterations / 4), monteCarloPi(iterations / 4)),
      parallel(monteCarloPi(iterations / 4), monteCarloPi(iterations / 4))
    )
    (pi1 + pi2 + pi3 + pi4) / 4
  }

  def main(args: Array[String]) {
    val iterations = 4000000
    val seqtime = standardConfig measure {
      seqResult = monteCarloPi(iterations)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      parResult = parMonteCarloPi(iterations)
    }
    println(s"fork/join time: $partime ms")
    println(s"speedup: ${seqtime/partime}")
    println(s"values computed are $seqResult vs $parResult")
  }

}
