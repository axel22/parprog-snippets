package lectures
package dataparallelism

import scala.collection._

object ParallelTrieMapGraphContraction {

  def main(args: Array[String]) {
    val graph = concurrent.TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
    graph(graph.size - 1) = 0
    val previous = graph.snapshot()
    for ((k, v) <- graph.par) graph(k) = previous(v)
    val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
    println(s"violation: $violation")
  }

}
