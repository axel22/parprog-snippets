package lectures

import scala.collection.parallel._
import scala.collection.mutable.ArrayBuffer

package object dataparallelism {

  class ParString(val str: String)
  extends immutable.ParSeq[Char] {
    
    def apply(i: Int) = str.charAt(i)
    
    def length = str.length
    
    def seq = new collection.immutable.WrappedString(str)
    
    def splitter = new ParStringSplitter(str, 0, str.length)
    
    class ParStringSplitter(private var s: String, private var i: Int, private val ntl: Int)
    extends SeqSplitter[Char] {
      final def hasNext = i < ntl
      final def next = {
        val r = s.charAt(i)
        i += 1
        r
      }
      def remaining = ntl - i
      def dup = new ParStringSplitter(s, i, ntl)
      def split = {
        val rem = remaining
        if (rem >= 2) psplit(rem / 2, rem - rem / 2)
        else Seq(this)
      }
      def psplit(sizes: Int*): Seq[SeqSplitter[Char]] = {
        val splitted = new ArrayBuffer[ParStringSplitter]
        for (sz <- sizes) {
          val next = (i + sz) min ntl
          splitted += new ParStringSplitter(s, i, next)
          i = next
        }
        splitted
      }
    }
    
  }

}
