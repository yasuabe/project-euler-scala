package pe

import util.Primes._

object Pe003 {
  def divide(n: Long, ps: Stream[Long]): Long = {
    val p #:: pt = ps
    if (n == p) p
    else if (n < p * p) n
    else if (n % p == 0) divide(n / p, ps)
    else divide(n, pt)
  }
  def main(args: Array[String]) = run(divide(600851475143L, primes()))
}
