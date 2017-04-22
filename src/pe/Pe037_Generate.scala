package pe

import java.math.BigInteger

object Pe037_Generate extends BaseApp[Int] {
  val rightmosts = Set(1, 3, 7, 9)
  def isPrime5(n: Int): Boolean = BigInteger.valueOf(n).isProbablePrime(5)
  def leftTruncatable(n: Int): Int = {
    def loop(t: Int): Boolean = n < t || isPrime5(n % t) && loop(t * 10)
    if (loop(10)) n else 0
  }
  def sumFrom(left: Int): Int =
    if (!isPrime5(left)) 0
    else rightmosts.map(n => sumFrom(left * 10 + n)).sum + leftTruncatable(left)

  def main: Int =
    Set(2, 3, 5, 7).flatMap(n => rightmosts.map(n * 10 + _)).map(sumFrom).sum
}
