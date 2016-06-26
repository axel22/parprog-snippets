package lectures.algorithms

import org.scalatest.FunSuite

import scala.util.Random

class MergeSortTest extends FunSuite {

  def sortCase(nbElem: Int, maxDepth: Int) = {
    print(s"Test case: # elements = $nbElem, maxDepth = $maxDepth")
    val arr2sort = Array.tabulate(nbElem)(_ => Random.nextInt())
    val expected = arr2sort.sorted
    MergeSort.parMergeSort(arr2sort, maxDepth)
    assert(arr2sort === expected)
    println(" => ok")
  }

  /**
   * Note that maxDepth should not be too large (< 15),
   * otherwise number of thread in parallel will be exponential to maxDepth
   * which make the system slow
   */
  test("MergeSort x elements with maxDepth = y") {
    for {
      x <- 100 to 1000 by 100
      y <- 2 to 5
    } sortCase(x, y)
  }

}
