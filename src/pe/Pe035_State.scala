package pe

import cats.data.State
import cats.data.State._
import util.Primes

import scala.collection.SortedSet

object Pe035_State {
  def initialBits: SortedSet[Int] = Primes.primes()
    .dropWhile(_ <= 11)
    .takeWhile(_ < 1000000)
    .foldLeft(SortedSet.empty[Int])(_ + _.toInt)

  def nextRotation(number: Int, tens: Int): Int = {
    val (d, m)  =  divMod(number, 10)
    (m * tens + d).toInt
  }
  def rotate(number: Int, count: Int, tens: Int): State[SortedSet[Int], Boolean] =
    if (count == 0) pure(true)
    else for {
      current <- inspect[SortedSet[Int], Boolean](_.contains(number))
      _       <- modify[SortedSet[Int]](s => if (current) s - number else s)
      next    <- rotate(nextRotation(number, tens), count - 1, tens)
    } yield current && next

  def addition(number: Int, count: Int, tens: Int): State[SortedSet[Int], Int] =
    rotate(number, count, tens).map(if (_) count else 0)

  def processPrimes(cols: Int, tens: Int, results: Int): State[SortedSet[Int], Int] =
    State.get.flatMap(
      _.headOption match {
        case None                           => pure[SortedSet[Int], Int](results)
        case Some(head) if head > tens * 10 => processPrimes(cols + 1, tens * 10, results)
        case Some(head)                     => addition(head, cols, tens).flatMap {
          a => processPrimes(cols, tens, results + a)
        }
      }
    )

  def solve = processPrimes(2, 10, 5).runA(initialBits).value

  def main(args: Array[String]): Unit = run(solve)
}
