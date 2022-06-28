package pe

import util.Primes._

object Pe010:
  def solve(limit: Int): Long = primes().takeWhile(_ < limit).sum

  @main def main010 = run(solve(2000000))
