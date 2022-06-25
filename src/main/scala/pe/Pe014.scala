package pe

import scala.annotation.tailrec

object Pe014 {
  def next(n: Long): Long = n % 2 match {
    case 0 => n / 2
    case _ => 3 * n + 1
  }
  @tailrec
  def terms(n: Long, len: Int): Int = n match {
    case 1 => len
    case _ => terms(next(n), len + 1)
  }
  def solve(limit: Int) = {
    (1 until limit).foldLeft((0, 0L)) { case ((m, acc), n) =>
      val t = terms(n, 1)
      if (t > acc) (n, t) else (m, acc)
    }
  }
  def main(args: Array[String]): Unit = run(solve(1000000))
}
