package pe

import util.Utils._

object Pe002 {
  def solve(a: Int, b: Int, sum: Long): Long =
    if (b >= 4000000) sum
    else solve(b, a + b, sum + (if (b % 2 == 0) b else 0))

  def main(args: Array[String]) = run(solve(1, 2, 0))
}
