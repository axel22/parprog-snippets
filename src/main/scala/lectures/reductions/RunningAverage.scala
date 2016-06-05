package lectures
package reductions

import org.scalameter._
import common._

object RunningAverage {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 60,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def runningAverage(input: Array[Int], output: Array[Float]): Unit = {
    var i = 0
    var xPrev = 0
    output(0) = xPrev
    while (i < input.length) {
      xPrev = input(i) + xPrev
      i += 1
      output(i) = 1.0f * xPrev / i
    }
  }

  sealed abstract class Tree {
    def xPrev: Int
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val xPrev = left.xPrev + right.xPrev
  }

  case class Leaf(from: Int, until: Int, xPrev: Int) extends Tree

  def parRunningAverage(input: Array[Int], output: Array[Float], threshold: Int): Unit = {
    def reduceSequential(from: Int, until: Int): Int = {
      var i = from
      var x = 0
      while (i < until) {
        x = input(i) + x
        i += 1
      }
      x
    }

    def reduce(from: Int, until: Int): Tree = {
      if (until - from < threshold) {
        Leaf(from, until, reduceSequential(from, until))
      } else {
        val mid = (from + until) / 2
        val (leftTree, rightTree) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        Node(leftTree, rightTree)
      }
    }

    val tree = reduce(0, input.length)

    def downsweepSequential(xPrev: Int, from: Int, until: Int): Unit = {
      var i = from
      var x = xPrev
      while (i < until) {
        x = input(i) + x
        i += 1
        output(i) = 1.0f * x / i
      }
    }

    def downsweep(xPrev: Int, tree: Tree): Unit = tree match {
      case Node(left, right) =>
        parallel(
          downsweep(xPrev, left),
          downsweep(xPrev + left.xPrev, right)
        )
      case Leaf(from, until, _) =>
        downsweepSequential(xPrev, from, until)
    }

    output(0) = 0
    downsweep(0, tree)
  }

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 - 50).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      runningAverage(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      parRunningAverage(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}
