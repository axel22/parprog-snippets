package lectures
package reductions

import org.scalameter._
import common._

object ArrayMap {

  def mapASegSeq[A,B](inp: Array[A], f : A => B,
		      left: Int, right: Int,
		      out: Array[B]) = {
    var i= left
    while (i < right) {
      out(i)= f(inp(i))
      i= i+1
    }
  }

  def mapASegPar[A,B](inp: Array[A], left: Int, right: Int,
		      f : A => B,
		      out: Array[B]): Unit = {
    // require f to be pure
    if (right - left < threshold)
      mapASegSeq(inp, f, left, right, out)
    else {
      val mid = left + (right - left)/2
      val _ = parallel(mapASegPar(inp, left, mid, f, out),
		       mapASegPar(inp, mid, right, f, out))
    }
  }

  def normsOfPar(inp: Array[Int], p: Double,
	         left: Int, right: Int,
	         out: Array[Double]): Unit = {
    if (right - left < threshold) {
      var i= left
      while (i < right) {
        out(i)= power(inp(i),p)
        i= i+1
      }
    } else {
      val mid = left + (right - left)/2
      val _ = parallel(normsOfPar(inp, p, left, mid, out),
		       normsOfPar(inp, p, mid, right, out))
    }
  }

  def normsOf(inp: Array[Int], p: Double,
	      left: Int, right: Int,
	      out: Array[Double]): Unit = {
      var i= left
      while (i < right) {
        out(i)= power(inp(i),p)
        i= i+1
      }
  }

  // an effectful map: more flexible, but easier to use wrongly
  def actionOnSegPar(action : Int => Unit,
		     left: Int, right: Int): Unit = {
    // require action(i1) and action(i2) do not interfere for i1 != i2
    if (right - left < threshold) {
      var i= left
      while (i < right) {
	action(i)
	i= i+1
      }
    } else {
      val mid = left + (right - left)/2
      val _ = parallel(actionOnSegPar(action, left, mid),
		       actionOnSegPar(action, left, mid))
    }
  }

  def mapASegPar2[A,B](inp: Array[A], left: Int, right: Int,
		       f : A => B,
		       out: Array[B]): Unit = {
    def action(i: Int): Unit = { out(i)= f(inp(i)) }
    actionOnSegPar(action, left, right)
  }

  val logE = math.log(math.E)

  def power(x: Int, p: Double): Int = {
    math.exp(p * math.log(math.abs(x)) / logE).toInt
  }

  def mapNormSeq(inp: Array[Int], p: Double, 
	         out: Array[Double]): Unit = {
    require(inp.length == out.length)    
    def f(x: Int): Double = power(x, p)
    mapASegSeq(inp, f, 0, inp.length, out)
  }

  def mapNormPar(inp: Array[Int], p: Double,
	         out: Array[Double]): Unit = {
    require(inp.length == out.length)
    def f(x: Int): Double = power(x, p)
    mapASegPar(inp, 0, inp.length, f, out)
  }

  def mapNormPar2(inp: Array[Int], p: Double,
	         out: Array[Double]): Unit = {
    require(inp.length == out.length)
    def f(x: Int): Double = power(x, p)
    mapASegPar2(inp, 0, inp.length, f, out)
  }

  val threshold = 10000

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 30,
    Key.exec.maxWarmupRuns -> 30,
    Key.exec.benchRuns -> 20,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val p = 1.5
    val alen = 2000000
    val inp = (0 until alen).map(_ % 100).toArray
    val out1 = (0 until alen).map(_ => 0.0).toArray
    val out2 = (0 until alen).map(_ => 0.0).toArray
    val out3 = (0 until alen).map(_ => 0.0).toArray
    val seqtime = standardConfig measure {
      mapNormSeq(inp, p, out1)
    }

/*
    val mapNormPar2time = standardConfig measure {
      mapNormPar2(inp, p, out3)
    }
    println(s"mapNormPar2: $mapNormPar2time ms")  
    println(s"speedup2: ${seqtime/mapNormPar2time}")
*/

    val mapNormParTime = standardConfig measure {
      mapNormPar(inp, p, out2)
    }

    val normsOfParTime = standardConfig measure {
      normsOfPar(inp, p, 0, inp.length, out3)
    }

    val normsOfTime = standardConfig measure {
      normsOf(inp, p, 0, inp.length, out3)
    }

    println(s"sequential sum time: $seqtime ms")
    println(s"mapNormPar time: $mapNormParTime ms")
    println(s"normsOfPar time: $normsOfParTime ms")
    println(s"normsOf time: $normsOfTime ms")
    /* Example output on Intel(R) Core(TM) i7-3770K CPU @ 3.50GHz (4 cores, 8 hw threads), 16GB RAM
[info] sequential sum time: 174.17463240000004 ms
[info] mapNormPar time:      28.9307023 ms
[info] normsOfPar time:      28.165657500000002 ms
[info] normsOf time:        166.83788205000002 ms

       Note that manual inlining does not pay off much,
       and parallelization is where the main win is!
*/
  }

}
