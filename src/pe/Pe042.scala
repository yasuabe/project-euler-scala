package pe

import java.math.BigInteger

import scala.collection.immutable.::

object Pe041 extends BaseApp[Int] {
  def maybePrime(ds: List[Int]): Option[Int] = {
    val n = ds.foldRight(0)(_ + _ * 10)
    if (Seq(1, 3, 7).contains(ds.head) && BigInteger.valueOf(n).isProbablePrime(5)) Some(n) else None
  }
  def solve(ns: List[Int]): Option[Int] = {
    def go(as: List[Int], bs: List[Int], cs: List[Int]): Option[Int] = {
      (bs, cs) match {
        case (Nil, _)        => None
        case (h :: Nil, Nil) => maybePrime(h :: as)
        case (h :: t,   _)   =>
          go(h :: as, cs.foldLeft(t)((acc, a) => a :: acc), Nil).orElse(go(as, t, h :: cs))
      }
    }
    go(Nil, ns, Nil)
  }
  def main: Int = solve(List(7, 6, 5, 4, 3, 2, 1)).get
}
