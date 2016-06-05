package lectures
package dataparallelism

import scala.collection._
import org.scalameter._

object IntersectionCorrect {

  def main(args: Array[String]) {
    def intersection(a: GenSet[Int], b: GenSet[Int]): GenSet[Int] = {
      if (a.size < b.size) a.filter(b(_))
      else b.filter(a(_))
    }
    val seqres = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
    val parres = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
    log(s"Sequential result - ${seqres.size}")
    log(s"Parallel result   - ${parres.size}")
  }
  
}
