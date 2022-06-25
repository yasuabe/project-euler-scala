package pe

import scala.collection.SortedSet

object Pe038 extends BaseApp[Int] {
  val oneToEight: SortedSet[Int] = SortedSet(1 to 8:_*)

  def isPalindromic(x: Int, result: Int = 0): Boolean =
    if (x == 0) result == 1022 else isPalindromic(x / 10, result | (1 << (x % 10)))

  def calc(n: Int): Option[Int] = Some(n * 100000 + n * 2).filter(isPalindromic(_))

  def main: Int = {
    def rec(n:Int = 0, d: Int = 3, ns: SortedSet[Int] = oneToEight): Seq[Int] = {
      ns.toSeq.flatMap { m =>
        val next = n * 10 + m
        if (d == 1) calc(9000 + next) else rec(next, d - 1, ns - m)
      }
    }
    rec(0, 3, oneToEight).max
  }
}
