package pe

import scala.annotation.tailrec

object Pe034_2 {
  val factorials = List(1, 2, 3, 4, 5, 6, 7, 8, 9).scan(1)(_ * _)

  @tailrec def sumFact(n: Int, sum: Int = 0): Int =
    if (n == 0) sum else sumFact(n / 10, sum + factorials(n % 10))

  def scan(length: Int, prevSum: Int = 0)(set: Set[Int], n: Int): Set[Int] = {
    val factSum = prevSum + factorials(n)
    val set2    = test(factSum, set)

    if (length == 0) set2
    else (n to 9).foldLeft(set2)(scan(length - 1, factSum))
  }
  def test(factSum: Int, set: Set[Int]) = if (sumFact(factSum) == factSum) set + factSum else set

  def solve(max: Int) = (0 to 9).foldLeft(Set.empty[Int])(scan(max)).sum - 3

  def main(args: Array[String]): Unit = run(solve(6))
}
