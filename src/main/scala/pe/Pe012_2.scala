package pe

import util.LazyPrimes

import scala.annotation.tailrec

// memoized version (about twice faster)
object Pe012_2:
  val primes = LazyPrimes.primes().take(10000).tail.toList
  val memo   = Array.fill(30000, 2)(0)

  def countPrimes(n: Int, p: Long): (Int, Int) =
    @tailrec def go(current: Int, count: Int): (Int, Int) = 
      if p divides current then go(current / p.toInt, count + 1) else (current, count)
    go(n, 0)

  def countFactors(n: Int): Int =
    @tailrec def go(current: Int, currentCount: Int, ps: List[Long]): Int =
      val ph :: pt          = ps
      val (next, nextCount) = countPrimes(current, ph)
      val combinations      = (nextCount + 1) * currentCount

      if 1 == next then combinations else go(next, combinations, pt)

    val result = go(n, 1, primes)
    memo(n)(0) = result
    result

  @tailrec def loop(even: Int, isEven: Boolean): Long =
    if isEven then
      val odd   = even * 2 + 1
      val count = memo(even)(0) * memo(even)(1) * countFactors(odd)
      if count > 500 then even * odd
      else
        memo(even * 2)(0) = memo(even)(0)
        memo(even * 2)(1) = memo(even)(1) + 1
        loop(even + 1, false)
    else
      val odd = even * 2 - 1
      val count = memo(even)(0) * memo(even)(1) * memo(odd)(0)
      if count > 500 then even * odd else loop(even, true)

  def solve: Long =
    for (i <- 0 until 30000) memo(i)(1) = 1
    memo(1)(0) = 1
    memo(2)(0) = memo(1)(0)
    memo(2)(1) = memo(1)(0) + 1
    loop(1, true)

  @main def main012_2 = run(solve)
