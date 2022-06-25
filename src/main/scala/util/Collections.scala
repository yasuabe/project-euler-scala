package util

import scala.collection.immutable.Stream.cons

object Collections {
  def unfold[S, A](init: S)(f: S => (A, S)): Stream[A] = {
    val (a, s) = f(init)
    cons(a, unfold(s)(f))
  }
}
