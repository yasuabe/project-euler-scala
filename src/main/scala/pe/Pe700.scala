package pe

import scala.annotation.tailrec

object Pe700:
  def solve(mod: Long, first: Long): Long =
    @tailrec def loop(last: Long, diff: Long, prev: Long): Long =
      val (n, a) = divMod(last, diff)
      val acc    = prev + n * (2 * last - (n + 1) * diff) / 2
      if a == 0 then acc else loop(a, diff % a, acc)

    loop(first, mod % first, first)

  @main def main700 = println(solve(4503599627370517L, 1504170715041707L))
