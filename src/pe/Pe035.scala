package pe

import util.Primes
import scala.collection.SortedSet

object Pe035 {
  var bits: SortedSet[Int] = SortedSet.empty[Int]
  def initialBits: SortedSet[Int] = {
    Primes.primes().dropWhile(_ <= 11).takeWhile(_ < 1000000).foldLeft(SortedSet.empty[Int])((acc, e) => acc + e.toInt)
  }
  def processRotate(number: Int, count: Int, tens: Int, result: Boolean = true): Boolean =
    if (count == 0) result
    else {
      val current = bits.contains(number)
      bits = if (current) bits - number else bits
      val (d, m)  = divMod(number, 10)
      val next    = m * tens + d
      processRotate(next.toInt, count - 1, tens, current && result)
    }

  def rotate(cols: Int, tens: Int, results: Int): Int = bits.headOption match {
    case None                           => results
    case Some(head) if head > tens * 10 => rotate(cols + 1, tens * 10, results)
    case Some(head)                     =>
      rotate(cols, tens, results + (if (processRotate(head, cols, tens)) cols else 0))
  }
  def solve = {
    bits = initialBits
    rotate(2, 10, 5)
  }

  def main(args: Array[String]): Unit = run(solve)
}
