package lectures
package reductions

import org.scalameter._
import common._

object ArrayScanDebug {

  def foldASegSeq[A,B](inp: Array[A],  
                       left: Int, right: Int,
		       b0: B,
                       f: (B,A) => B): B = {
    var b= b0
    var i= left
    while (i < right) {
      b= f(b, inp(i))
      i= i+1
    }
    b
  }

  sealed abstract class FoldTree[A] {
    val res: A
  }
  case class Leaf[A](from: Int, to: Int, resLeaf: A) extends FoldTree[A] {
    val res= resLeaf
  }
  case class Node[A](l: FoldTree[A], r: FoldTree[A], resNode: A) extends FoldTree[A] {
    val res= resNode
  }

  def foldASegParTree[A](inp: Array[A], 
                         left: Int, right: Int,
			 a0: A, 
                         f: (A,A) => A): FoldTree[A] = {
    // requires f to be associative
    if (right - left < threshold)
      Leaf(left, right, foldASegSeq(inp, left, right, a0, f))
    else {
      val mid = left + (right - left)/2
      val (t1,t2) = parallel(foldASegParTree(inp, left, mid, a0, f),
                             foldASegParTree(inp, mid, right, a0, f))
      Node(t1, t2, f(t1.res,t2.res))
    }
  }

/* // Poor man's dynamic effect checks
  def write[A](arr: Array[A], i: Int, v:A) = {
    if (arr(i) != 0.0) {
      println(s"Overwritting with $v element at $i already set to ${arr(i)} in array ${printA(arr)}")
    }
    arr(i)=v
  }
*/

  def scanASegSeq1[A](inp: Array[A], 
                     left: Int, right: Int,
                     a0: A,
                     f: (A,A) => A,
                     out: Array[A]) = {
    if (left < right) {
      var i= left
      var a= a0
      while (i < right) {
        a= f(a,inp(i))
        //write(out,i+1,a)
	out(i+1)=a
        i= i+1
      }
    }
  }

  def scanASegParT[A,B](inp: Array[A], 
                        a0: A,
                        f: (A,A) => A,
                        t: FoldTree[A],
                        out: Array[A]): Unit = {
    t match {
      case Leaf(from, to, res) => 
        scanASegSeq1(inp, from, to, a0, f, out)
      case Node(l, r, res) => {
        val (_,_) = parallel(
          scanASegParT(inp, a0, f, l, out),
          scanASegParT(inp, f(a0,l.res), f, r, out))
      }
    }
  }

  def scanASegPar[A,B](inp: Array[A], 
                       from: Int, to: Int,
                       a0: A,
                       f: (A,A) => A,
                       out: Array[A]) = {
    val t = foldASegParTree(inp, from, to, a0, f)
    println("FoldTree is: " + t)
    scanASegParT(inp, a0, f, t, out)
  }

  def scanAPar[A](inp: Array[A], 
                  a0: A,
                  f: (A,A) => A,
                  out: Array[A]) = {
    out(0)= a0
    scanASegPar(inp, 0, inp.length, a0, f, out)
  }

  def scanASeq[A](inp: Array[A], 
                  a0: A,
                  f: (A,A) => A,
                  out: Array[A]) = {
    out(0) = a0
    scanASegSeq1(inp, 0, inp.length, a0, f, out)
  }

  val c = 2.99792458e8 
  def assocOp(v1: Double, v2: Double): Double = {
    val u1 = v1/c
    val u2 = v2/c
    (u1 + u2)/(1 + u1*u2)*c
  }

  def sum(x: Double, y: Double) = x + y

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 1,
    Key.exec.maxWarmupRuns -> 1,
    Key.exec.benchRuns -> 1,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def printA[A](a: Array[A]): String = {
    a.toList.toString
  }

  val threshold = 3 

  def main(args: Array[String]) {
    //    val inp = (0 until alen).map((x:Int) => (x % 50)*0.0001*c).toArray
    val inp = Array(1.0, 10.0, 200.0, 0.5, 3.0, 40.0, 50.0, 5.0)
    val alen = inp.length
    println("Input: " + printA(inp))
    val outSeq = new Array[Double](alen + 1); outSeq(0) = 41.0; outSeq(alen) = -12.0
    val outPar = new Array[Double](alen + 1); outPar(0) = 42.0; outPar(alen) = -13.0
    val seqtime = 1
    //standardConfig measure {
    scanASeq[Double](inp, 0.0, sum, outSeq)
    //}
    //println(s"sequential time: $seqtime ms and result ${printA(outSeq)}")

    val partime = 1
    //standardConfig measure {
      scanAPar[Double](inp, 0.0, sum, outPar)
    //}
    //println(s"parallel time: $partime ms and result ${printA(outPar)}")
    //println(s"speedup: ${seqtime / partime}")
    println(s"seq result ${printA(outSeq)}")
    println(s"par result ${printA(outPar)}")

  }

}
