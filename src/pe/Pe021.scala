package pe

import util.Primes._

object Pe021 extends UseFraction {
  val primes1000 = primes().takeWhile(_ < 1000)

  def factorSum(primeFactors: List[(Long, Int)]): Long = {
    val ls = primeFactors.map{
      case (p, e) => frac(pow(p, e + 1) - 1, p - 1)
    }.foldLeft((1L, 1L))((acc, e) => multiply(acc, e))
    ls._1
  }

  def properDivisorSum(n: Long): Long = factorSum(primeFactors(n, primes1000)) - n

  def solve(n: Int): Int = {
    (2 to 10000).filter { n =>
      val m = properDivisorSum(n)
      m != n && n == properDivisorSum(m)
    }.sum
  }
  def main(args: Array[String]): Unit = run(solve(10000))
}
