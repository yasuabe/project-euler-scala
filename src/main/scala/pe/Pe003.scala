package pe

import scala.annotation.tailrec
import util.LazyPrimes.*

object Pe003:
  def solve(limit: Long): Long =
    @tailrec def divide(n: Long, ps: LazyList[Long]): Long =
      val p #:: pt = ps
      if      n == p     then p
      else if n < p * p  then n
      else if n % p == 0 then divide(n / p, ps)
                         else divide(n,     pt)
    divide(limit, primes())

  @main def main003 = run(solve(600851475143L))
