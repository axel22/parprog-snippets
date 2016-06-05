package lectures

package object algorithms {

  implicit class ConcOps[T](val self: Conc[T]) extends AnyVal {
    def foreach[U](f: T => U) = Conc.traverse(self, f)
    def <>(that: Conc[T]) = Conc.concatTop(self.normalized, that.normalized)
  }

}