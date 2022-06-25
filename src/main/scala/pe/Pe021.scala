package pe

import util.Primes._
import util.UseFactors

object Pe021 extends UseFraction with UseFactors {
  val primes1000 = primes().takeWhile(_ < 1000)

  def solve(n: Int): Int = {
    (2 to 10000).filter { n =>
      val m = properDivisorSum(n, primes1000)
      m != n && n == properDivisorSum(m, primes1000)
    }.sum
  }
  def main(args: Array[String]): Unit = run(solve(10000))
}
