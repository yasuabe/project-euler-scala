package pe

import scala.annotation.tailrec
import util.LazyPrimes._

object Pe005:
  def maxPowIn(prime: Long, limit: Long): Long =
    @tailrec
    def loop(pow: Long): Long = if pow * prime > limit then pow else loop(pow * prime)
    loop(prime)

  def solve(n: Int): Long = primes().takeWhile(_ <= n).map(maxPowIn(_, n)).product

  @main def main005 = run(solve(20))
