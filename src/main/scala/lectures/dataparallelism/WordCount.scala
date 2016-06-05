package lectures
package dataparallelism

import org.scalameter._

object WordCount {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 50,
    Key.exec.maxWarmupRuns -> 100,
    Key.exec.benchRuns -> 40,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)
  
  val txt = "A short text...  " * 250000
  val ps = new ParString(txt)
  
  def main(args: Array[String]) {
    val seqtime = standardConfig measure {
      txt.foldLeft((0, true)) {
        case ((wc, _), ' ') => (wc, true)
        case ((wc, true), x) => (wc + 1, false)
        case ((wc, false), x) => (wc, false)
      }
    }
    println(s"sequential time: $seqtime ms")
    
    val partime = standardConfig measure {
      ps.aggregate((0, 0, 0))({ (x, y) =>
        if (x._2 > 0) {
          if (y != ' ') x match {
            case (ls, wc, 0)  => (ls, wc, 0)
            case (ls, wc, rs) => (ls, wc + 1, 0)
          } else x match {
            case (ls, wc, rs) => (ls, wc, rs + 1)
          }
        } else {
          if (y != ' ') x match {
            case (ls, 0, _) => (ls + 1, 0, ls + 1)
          } else x match {
            case (ls, 0, _) => (ls + 1, 1, 0)
          }
        }
      }, {
        case ((0, 0, 0), res) => res
        case (res, (0, 0, 0)) => res
        case ((lls, lwc, 0), (0, rwc, rrs)) => (lls, lwc + rwc - 1, rrs)
        case ((lls, lwc, _), (_, rwc, rrs)) => (lls, lwc + rwc, rrs)
      })
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
  
}
