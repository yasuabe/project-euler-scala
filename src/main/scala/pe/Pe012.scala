package pe

import scala.annotation.tailrec
import util.LazyPrimes

object Pe012:
  val primeList = LazyPrimes.primes().take(10000).toList
  def countPrimes(n: Long, p: Long): (Long, Long) =
    @tailrec def go(current: Long, count: Long): (Long, Long) =
      if p divides current then go(current / p, count + 1) else (current, count)

    go(n, 0L)

  def countFactors(n: Long): Long =
    @tailrec def go(current: Long, currentCount: Long, ps: List[Long]): Long =
      val ph :: pt          = ps
      val (next, nextCount) = countPrimes(current, ph)
      val combinations      = (nextCount + 1) * currentCount

      if 1 == next then combinations else go(next, combinations, pt)

    go(n, 1L, primeList)

  def solve(limit: Long): Option[Long] =
    LazyList
      .iterate((1, 1))((a, b)     => (a + 1, a + b + 1))
      .map            ((_, tr)    => (tr, countFactors(tr)))
      .find           ((_, count) => count >= limit)
      .map(_._1)

  @main def main012 = run(solve(500))
