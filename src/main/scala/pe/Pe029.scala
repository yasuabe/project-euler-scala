package pe

import scala.annotation.tailrec
import scala.collection.{BitSet, SortedSet}

object Pe029 {
  def solve(limit: Int) = {
    def f1(e: Int, s: BitSet): BitSet = ((limit / e + 1) to limit).foldLeft(s)(_ + _ * e)

    def f2(n: Int, rows: SortedSet[Int]): (BitSet, SortedSet[Int]) = Stream
      .iterate((2, n * n)) { case (e, p) => (e + 1, p * n) }
      .takeWhile { case (_, p) => p <= limit }
      .foldLeft((BitSet(2 to limit: _*), rows)) { case ((es, rs), (e, r)) => (f1(e, es), rs - r) }

    @tailrec def f3(sum: Int, ns: SortedSet[Int]): Int =
      if (ns.head > sqrt(limit)) sum + ns.size * (limit - 1)
      else {
        val (exponents, ms) = f2(ns.head, ns.tail)
        f3(sum + exponents.size, ms)
      }

    f3(0, SortedSet(2 to limit :_*))
  }
  def main(args: Array[String]): Unit = run(solve(100))
}
