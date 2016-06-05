package lectures
package dataparallelism

import org.scalameter._
import scala.collection._

object ParallelMutation {

  def main(args: Array[String]) {
    val array = Array.fill(10000000)("")
    val (result, _) = common.parallel(
      array.par.count(_ == ""),
      for (i <- (0 until 10000000).par) array(i) = "modified"
    )
    println(s"result: $result")
  }
}
