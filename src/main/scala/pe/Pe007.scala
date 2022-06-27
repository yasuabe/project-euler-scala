package pe

import util.LazyPrimes.*

object Pe007:
  def solve(limit: Int): Long = primes().drop(limit - 1).head

  @main def main007 = run(solve(10001))
