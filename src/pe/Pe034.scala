package pe

import scala.annotation.tailrec

object Pe034 {
  val factorials = List(1, 2, 3, 4, 5, 6, 7, 8, 9).scan(1)(_ * _)
  @tailrec def factSum(m: Long, result: Long): Long = divMod(m, 10) match {
    case (0, 0) => result
    case (d, m) => factSum(d, result + factorials(m.toInt))
  }
  def solve = (3 to 9999999).filter(n => n == factSum(n, 0)).sum

  def main(args: Array[String]): Unit = run(solve)
}
