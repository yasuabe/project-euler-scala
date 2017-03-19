package pe

import scala.annotation.tailrec
import util.Primes._

object Pe005 {
  def maxPowIn(prime: Long, limit: Long): Long = {
    @tailrec
    def loop(pow: Long): Long = if (pow * prime > limit) pow else loop(pow * prime)
    loop(prime)
  }
  def solve(n: Int): Long = primes().takeWhile(_ <= n).map(maxPowIn(_, n)).product

  def main(args: Array[String]) = run(solve(20))
}
