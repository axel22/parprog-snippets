package lectures
package reductions

import org.scalameter._
import common._

object TreeMap {

  sealed abstract class Tree[A] { val size: Int }
  case class Leaf[A](a: Array[A]) extends Tree[A] {
    override val size = a.size
  }
  case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A] {
    override val size = l.size + r.size
  }

  def mapTreeSeq[A:Manifest,B:Manifest](t: Tree[A], f: A => B) : Tree[B] = t match {
    case Leaf(a) => {
      val len = a.length
      val b = new Array[B](len)
      var i= 0
      while (i < len) {
	b(i)= f(a(i))
	i= i + 1
      } 
      Leaf(b)
    }
    case Node(l,r) => {
      val (lb,rb) = (mapTreeSeq(l,f),mapTreeSeq(r,f))
      Node(lb, rb)
    }
  }

  def mapTreePar[A:Manifest,B:Manifest](t: Tree[A], f: A => B) : Tree[B] = t match {
    case Leaf(a) => {
      val len = a.length
      val b = new Array[B](len)
      var i= 0
      while (i < len) {
	b(i)= f(a(i))
	i= i + 1
      } 
      Leaf(b)
    }
    case Node(l,r) => {
      val (lb,rb) = parallel(mapTreePar(l,f),mapTreePar(r,f))
      Node(lb, rb)
    }
  }

  val logE = math.log(math.E)
  def power(x: Double, p: Double): Int = {
    math.exp(p * math.log(math.abs(x)) / logE).toInt
  }

  val threshold = 10000

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 30,
    Key.exec.maxWarmupRuns -> 30,
    Key.exec.benchRuns -> 20,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)


  def makeTree(len: Int) : Tree[Double] = {
    if (len < threshold)
      Leaf((0 until len).map((x:Int) => (x % 100)*0.9).toArray)
    else {
      Node(makeTree(len/2), makeTree(len - len/2))
    }
  }

  def main(args: Array[String]) {
    val p = 1.5
    def f(x:Double)= power(x,p)
    val alen = 2000000
    val t = makeTree(alen)
    var t1: Tree[Double] = t
    var t2: Tree[Double] = t
    val seqtime = standardConfig measure {
      t1 = mapTreeSeq(t, f)
    }
    val partime = standardConfig measure {
      t2 = mapTreePar(t, f)
    }

    println(s"sequential time: $seqtime ms")
    println(s"parallel time: $partime ms")
  }

}
