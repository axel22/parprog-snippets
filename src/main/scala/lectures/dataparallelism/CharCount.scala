package lectures
package dataparallelism

import org.scalameter._

object CharCount {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  val txt = "A short text..." * 500000
  val ps = new ParString(txt)
  
  def main(args: Array[String]) {
    val seqtime = standardConfig measure {
      txt.foldLeft(0)((x, y) => x + 1)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      ps.aggregate(0)((x, y) => x + 1, _ + _)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
  
}
