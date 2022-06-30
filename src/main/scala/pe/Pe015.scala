package pe

import cats.syntax.functor.*
import util.homogeneoustuple.{ Pair, given }

object Pe015:
  type Fraction = Pair[Long]

  val reduce = (t: Fraction) => t match
    case (n, d) => t.fmap(_ / (n gcd d))

  val multiply: (Fraction, Fraction) => Fraction =
    (f1, f2) => reduce(f1._1 * f2._1, f1._2 * f2._2)

  def solve(limit: Int): Long =
    (1L to limit).toList.fproduct(_ + limit).map(reduce).reduce(multiply)._2

  @main def main015: Unit = run(solve(20))
