package lectures
package reductions
import org.scalameter._
import common._

object ArrayScan { // Parallel scan of an array

  /* 
   fold left array segment from left to right-1, sequentially.
   Used in the base case for upsweep.
   This is the same operation we would use in the base case of parallel fold.
   */
  def foldASegSeq[A,B](inp: Array[A],  
                       left: Int, right: Int,
		       b0: B, // initial element
                       f: (B,A) => B): B = {
    var b= b0
    var i= left
    while (i < right) {
      b= f(b, inp(i))
      i= i+1
    }
    b
  }

  // Binary trees whose nodes store elements of type A
  sealed abstract class FoldTree[A] {
    val res: A // whether it is leaf or internal node, res stores the result
  }
  case class Leaf[A](from: Int, to: Int, resLeaf: A) extends FoldTree[A] {
    val res= resLeaf
  }
  case class Node[A](l: FoldTree[A], r: FoldTree[A], resNode: A) extends FoldTree[A] {
    val res= resNode
  }

  /* 
   fold array segment in parallel and record the intermediate computation results in a Tree[A].
   In the context of scan, this phase is called upsweep.
   For an intuition, picture the array to reduce on the bottom, and the root of the tree at the top.
   Once the 'parallel' tasks are initiated, the results are combined in the 'up' direction, from array
   to the result of the fold.
   */
  def upsweep[A](inp: Array[A], 
                 left: Int, right: Int,
	 	 a0: A, 
	 	 f: (A,A) => A): FoldTree[A] = {
    // requires f to be associative
    if (right - left < threshold)
      Leaf(left, right, foldASegSeq(inp, left + 1, right, inp(left), f))
    else {
      val mid = left + (right - left)/2
      val (t1,t2) = parallel(upsweep(inp, left, mid, a0, f),
                             upsweep(inp, mid, right, a0, f))
      Node(t1, t2, f(t1.res,t2.res))
    }
  }

  /* 
   Scan array segment inp(left) to inp(right-1),
   storing results into out(left+1) to out(right).
   At the end, out(i+1) stores fold of elements:
   [a0, in(left),... in(i)] for i from left to right-1.
   In particular, out(left+1) stores f(a0,inp(left))
   and out(right) stores fold of [a0, in[(left),... inp(right-1)].
   The value a0 is not directly stored into out anywhere.

   This is used below cutoff in downsweep for scanAPar,
   and also to implement scanASeq as a comparison point.
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
	out(i+1)=a
        i= i+1
      }
    }
  }

  def downsweep[A](inp: Array[A], 
                   a0: A,
                   f: (A,A) => A,
                   t: FoldTree[A],
                   out: Array[A]): Unit = {
    t match {
      case Leaf(from, to, res) => 
        scanASegSeq1(inp, from, to, a0, f, out)
      case Node(l, r, res) => {
        val (_,_) = parallel(
          downsweep(inp, a0, f, l, out),
          downsweep(inp, f(a0,l.res), f, r, out))
      }
    }
  }

  def scanASegPar[A](inp: Array[A], 
                     from: Int, to: Int,
                     a0: A,
                     f: (A,A) => A,
                     out: Array[A]) = {
    val t = upsweep(inp, from, to, a0, f)
    downsweep(inp, a0, f, t, out)
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

  /* 
   =======================================
   Setting parameters and testing 
   =======================================
   */

  var threshold = 20000

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 6,
    Key.exec.maxWarmupRuns -> 6,
    Key.exec.benchRuns -> 5,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)


  def testConcat : Unit = {
    println("===========================================")
    println("Testing ArrayScan on concatenation example.")
    println("===========================================")

    def concat(x: List[Int], y: List[Int]): List[Int] =
      x ::: y

    def arrEq[A](a1: Array[A], a2: Array[A]): Boolean = {
      def eqSeq(from: Int, to: Int): Boolean = {
	var i= from
	while (i < to) {
	  if (a1(i) != a2(i)) {
	    println(s"Array difference: a1(${i})=${a1(i)}, a2(${i})=${a2(i)}")
	    return false
	  } else {
	    i= i + 1
	  }
	}
	true
      }
      if (a1.length != a2.length) {
	println("Different sizes!")
	false
      } else eqSeq(0, a1.length)
    }

    threshold = 100

    val alen = 2000
    val inp = (0 until alen).map((x:Int) => List(x)).toArray
    val outSeq = new Array[List[Int]](alen + 1)
    val outPar = new Array[List[Int]](alen + 1)
    val init = List(12309, 32123)
    val seqtime = standardConfig measure {
      scanASeq(inp, init, concat, outSeq)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      scanAPar(inp, init, concat, outPar)
    }    
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
    print("Are results equal?")
    println(arrEq(outSeq, outPar))
    //println(outPar.toList)
  }

  def testVelocity = {
    println("======================================")
    println("Testing ArrayScan on velocity example.")
    println("======================================")

    threshold = 20000

    val c = 2.99792458e8 
    def velocityAdd(v1: Double, v2: Double): Double = {
      val u1 = v1/c
      val u2 = v2/c
      (u1 + u2)/(1 + u1*u2)*c
    }

    val alen = 2000000
    val inp = (0 until alen).map((x:Int) => (x % 50)*0.0001*c).toArray
    val outSeq = new Array[Double](alen + 1)
    val outPar = new Array[Double](alen + 1)
    val seqtime = standardConfig measure {
      scanASeq(inp, 0.0, velocityAdd, outSeq)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      scanAPar(inp, 0.0, velocityAdd, outPar)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

  def testNonZero = {
    println("====================================================")
    println("Testing ArrayScan on addition with non-zero initial.")
    println("====================================================")
    val inp: Array[Int] = (1 to 10).toArray
    val outSeq: Array[Int] = new Array[Int](inp.length + 1)
    val outPar: Array[Int] = new Array[Int](inp.length + 1)
    val f = (x: Int, y: Int) => x + y
    threshold = 3
    scanASeq(inp, 10, f, outSeq)
    println(outSeq.toList)
    scanAPar(inp, 10, f, outPar) // a0 = 10
    println(outPar.toList)
  }

  def main(args: Array[String]) {    
    testNonZero
    testConcat
    testVelocity
  }
}
