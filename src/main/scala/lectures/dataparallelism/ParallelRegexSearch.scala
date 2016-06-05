package lectures
package dataparallelism

import org.scalameter._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection._
import scala.io.Source

object ParallelRegexSearch {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 30,
    Key.exec.maxWarmupRuns -> 60,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def getHtmlSpec() = Future {
    val specSrc: Source = Source.fromURL("http://www.w3.org/MarkUp/html-spec/html-spec.txt")
    try specSrc.getLines.toArray finally specSrc.close()
  }

  def main(args: Array[String]) {
    val measurements = for (specDoc <- getHtmlSpec()) yield {
      println(s"Download complete!")

      def search(d: GenSeq[String]) = standardConfig measure {
        d.indexWhere(line => line.matches(".*TEXTAREA.*"))
      }

      val seqtime = search(specDoc)
      val partime = search(specDoc.par)

      (seqtime, partime)
    }
    println("Fetching HTML specification, searching for TEXTAREA.")
    val (seqtime, partime) = Await.result(measurements, Duration.Inf)
    println(s"Sequential time $seqtime ms")
    println(s"Parallel time $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}
