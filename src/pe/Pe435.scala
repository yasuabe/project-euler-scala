package pe

import util.Utils.run

object Pe435 {
  val N = 1000000000000000L
  val M = BigInt(1307674368000L)
  val ZERO = BigInt(0)
  val ONE  = BigInt(1)
  val U = Seq(ONE, ONE,  ONE,  ZERO)
  val E = Seq(ONE, ZERO, ZERO, ONE)

  def main(args: Array[String]) = run(()=>solve)

  def solve: BigInt = {
    val n1 = BigInt(N + 1)
    val n2 = BigInt(N + 2)
    Range(1, 100 + 1).foldLeft(ZERO)((acc, i) => {
      val x   = BigInt(i)
      val den = BigInt(i * i + i - 1)
      val m   = M * den
      val (fib0, fib1) = fib(N, m)
      val num = fib0 * x.modPow(n2, m) + fib1 * x.modPow(n1, m) - x

      acc + num % m / den
    }) % M
  }
  def multiply(a: Seq[BigInt], b: Seq[BigInt], m: BigInt): Seq[BigInt] = {
    def multiply(c1: Int, r1: Int, c2: Int, r2: Int): BigInt =
       (a(c1) * b(r1) + a(c2) * b(r2)) % m
    Seq (
      multiply(0, 0, 1, 2), multiply(0, 1, 1, 3),
      multiply(2, 0, 3, 2), multiply(2, 1, 3, 3))
  }
  def fib(n: Long, m: BigInt): (BigInt, BigInt) = {
    val fibs = pow(U, n, m)
    (fibs(1), fibs(0))
  }
  def pow(a: Seq[BigInt], n: Long, m: BigInt) : Seq[BigInt] = n match {
    case 0 => E
    case 1 => a
    case _ =>
      val b = pow(a, n / 2, m)
      val c = multiply(b, b, m)
      if (n % 2 == 0) c else multiply(c, a, m)
  }
  def pow(a: BigInt, n: Long, m: BigInt): BigInt = a.modPow(BigInt(n), m)
}