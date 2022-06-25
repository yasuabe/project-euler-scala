package pe

import util.Primes

import scala.annotation.tailrec

// memoized version (about twice faster)
object Pe012_2 {
  val primes = Primes.primes().take(10000).tail.toList
  val memo   = Array.fill(30000, 2)(0)

  def countPrimes(n: Int, p: Long): (Int, Int) = {
    @tailrec def go(current: Int, count: Int): (Int, Int) = current % p match {
      case 0 => go(current / p.toInt, count + 1)
      case _ => (current, count)
    }
    go(n, 0)
  }
  def countFactors(n: Int): Int = {
    @tailrec def go(current: Int, currentCount: Int, ps: List[Long]): Int = {
      val ph :: pt = ps
      val (next, nextCount) = countPrimes(current, ph)
      val combinations      = (nextCount + 1) * currentCount

      if (1 == next) combinations
      else go(next, combinations, pt)
    }
    val result = go(n, 1, primes)
    memo(n)(0) = result
    result
  }
  @tailrec
  def loop(even: Int, isEven: Boolean): Long =
    if (isEven) {
      val odd = even * 2 + 1
      val count = 1L * memo(even)(0) * memo(even)(1) * countFactors(odd)
      if (count > 500) even * odd
      else {
        memo(even * 2)(0) = memo(even)(0)
        memo(even * 2)(1) = memo(even)(1) + 1
        loop(even + 1, isEven = false)
      }
    } else {
      val odd = even * 2 - 1
      val count = memo(even)(0) * memo(even)(1) * memo(odd)(0)
      if (count > 500) even * odd
      else loop(even, isEven = true)
    }
  def solve: Long = {
    for (i <- 0 until 30000) memo(i)(1) = 1
    memo(1)(0) = 1
    memo(2)(0) = memo(1)(0)
    memo(2)(1) = memo(1)(0) + 1
    loop(1, isEven = true)
  }
  def main(args: Array[String]) = run(solve)
}
