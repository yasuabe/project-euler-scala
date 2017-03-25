package pe

import util.Primes
import scala.annotation.tailrec

object Pe026 {
  def cycleLen(n: Long) = {
    @tailrec def go(n: Long, d: Long, ls: List[Long]): Int = n % d match {
      case 0                   => 1
      case m if ls.contains(m) => ls.takeWhile(_ != m).length + 1
      case m                   => go(m * 10, d, m :: ls)
    }
    go(1, n, Nil)
  }
  def solve(limit: Long) = Primes.primes().takeWhile(_ < limit).maxBy(cycleLen)

  def main(args: Array[String]): Unit = run(solve(1000))
}
