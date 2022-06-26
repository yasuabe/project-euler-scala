package pe

import scala.annotation.tailrec

object Pe004:
  extension (n: Int) def reverse: Int =
    @tailrec def loop(acc: Int, m: Int): Int =
      if m == 0 then acc else
        val (a, b) = divMod(m, 10)
        loop(acc * 10 + b, a)
    loop(0, n)

  def solve(n: Int): Int =
    @tailrec def s3(max: Int, row: Int, col: Int): Int =
      if row * row <= max then max else
        val prod = row * col
        if      prod <= max          then s3(max,  row - 1, row - 1)
        else if prod == prod.reverse then s3(prod, row - 1, row - 1)
                                     else s3(max,  row    , col - 1)
    s3(0, n, n)

  @main def main004 = run(solve(999))
