package lectures
package dataparallelism

import scala.collection._

object ParallelGraphContraction {

  def main(args: Array[String]) {
    val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
    graph(graph.size - 1) = 0
    for ((k, v) <- graph.par) graph(k) = graph(v)
    val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
    println(s"violation: $violation")
  }
}
