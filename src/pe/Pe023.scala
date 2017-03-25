package pe

import util.Primes._
import util.UseFactors

object Pe023 extends UseFraction with UseFactors {
  val primes1000000 = primes().takeWhile(_ < 1000000)

  def sum(n: Long): Long = (n + 1) * n / 2
  def solve(limit: Long) = {
    val tails =
      (2L to limit)
        .filter(n => properDivisorSum(n, primes1000000) > n)
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
