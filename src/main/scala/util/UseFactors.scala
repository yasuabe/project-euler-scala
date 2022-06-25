package util

import util.Primes.primeFactors
import pe.Pe023._
import pe._

trait UseFactors {
  def factorSum(primeFactors: List[(Long, Int)]): Long =
    primeFactors
      .map { case (p, e) => frac(pow(p, e + 1) - 1, p - 1)}
      .foldRight((1L, 1L))(multiply)
      ._1

  def properDivisorSum(n: Long, primes: Stream[Long]): Long = factorSum(primeFactors(n, primes)) - n
}
