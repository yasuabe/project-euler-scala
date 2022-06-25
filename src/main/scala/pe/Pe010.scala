package pe

import util.Primes._

object Pe010 {
  def solve(limit: Int) = primes().takeWhile(n => n < limit).sum

  def main(args: Array[String]) = run(solve(2000000))
}
