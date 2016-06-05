package lectures
package dataparallelism

import scala.collection._
import scala.collection.convert.wrapAsScala._
import java.util.concurrent._
import org.scalameter._

object IntersectionSynchronized {

  def main(args: Array[String]) {
    def intersection(a: GenSet[Int], b: GenSet[Int]) = {
      val result = new ConcurrentSkipListSet[Int]()
      for (x <- a) if (b contains x) result += x
      result
    }
    val seqres = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
    val parres = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
    log(s"Sequential result - ${seqres.size}")
    log(s"Parallel result   - ${parres.size}")
  }
  
}
