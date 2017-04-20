package pe

import java.math.BigInteger

import util.Primes

import scala.annotation.tailrec

object Pe037 {
  val bits = scala.collection.mutable.BitSet

  def isPrime(n: Int): Boolean = BigInteger.valueOf(n).isProbablePrime(5)
  @tailrec def loop(n: Int, tens: Int): Boolean = divMod2(n, tens) match {
    case (0, _)                                => true
    case (d, m) if !(isPrime(d) && isPrime(m)) => false
    case _                                     => loop(n, tens * 10)
  }
  def solve: Int =
    Primes.primes().dropWhile(_ < 10).map(_.toInt).filter(loop(_, 10)).take(11).sum

  def main(args: Array[String]): Unit = run(solve)
}
