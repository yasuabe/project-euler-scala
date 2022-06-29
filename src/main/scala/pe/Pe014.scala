package pe

import scala.annotation.tailrec
import mouse.boolean.*

object Pe014:
  def next(n: Long): Long = n.isEven.fold(n / 2, 3 * n + 1)

  @tailrec def terms(n: Long, len: Int): Int =
    if n == 1 then len else terms(next(n), len + 1)

  def solve(limit: Int) =
    (1 until limit).foldLeft((0, 0L)) { case ((m, acc), n) =>
      val t = terms(n, 1)
      if t > acc then (n, t) else (m, acc)
    }

  @main def main014: Unit = run(solve(1_000_000))
