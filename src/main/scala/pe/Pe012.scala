package pe

import util.Primes

import scala.annotation.tailrec

object Pe012 {
  val primeList = Primes.primes().take(10000).toList
  def countPrimes(n: Long, p: Long): (Long, Long) = {
    @tailrec def go(current: Long, count: Long): (Long, Long) = current % p match {
      case 0 => go(current / p, count + 1)
      case _ => (current, count)
    }
    go(n, 0L)
  }
  def countFactors(n: Long): Long = {
    @tailrec def go(current: Long, currentCount: Long, ps: List[Long]): Long = ps match {
      case ph :: pt =>
        val (next, nextCount) = countPrimes(current, ph)
        val combinations      = (nextCount + 1) * currentCount

        if (1 == next) combinations
        else go(next, combinations, pt)
    }
    go(n, 1L, primeList)
  }
  def solve(limit: Long): Option[Long] =
    Stream
      .iterate((1, 1)){ case (a, b)  => (a + 1, a + b + 1) }
      .map            { case (_, tr) => (tr, countFactors(tr)) }
      .find           { c => c._2 >= limit }
      .map(_._1)

  def main(args: Array[String]) = run(solve(500))
}
