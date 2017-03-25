package pe

import util.Primes._

object Pe023 extends UseFraction {
  val primes1000000 = primes().takeWhile(_ < 1000000)

  // TODO: 重複
  def factorSum(primeFactors: List[(Long, Int)]): Long =
    primeFactors.map {
      case (p, e) => frac(pow(p, e + 1) - 1, p - 1)
    }.foldRight((1L, 1L))(multiply)._1

  def properDivisorSum(n: Long): Long = factorSum(primeFactors(n, primes1000000)) - n

  def sum(n: Long): Long = (n + 1) * n / 2
  def solve(limit: Long) = {
    val tails =
      (2L to limit)
        .filter(n => properDivisorSum(n) > n)
        .toStream
        .tails
        .takeWhile(as => as.head * 2 <= limit)
        .foldLeft(Set.empty[Long]) {
          case (set, a@(b #:: c))  => a.map(_ + b).takeWhile(_ <= limit).foldLeft(set)(_ + _)
        }
    sum(limit) - tails.sum
  }
  def main(args: Array[String]): Unit = run(solve(28123))
}
