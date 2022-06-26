package pe

import cats.data.Reader
import cats.syntax.apply.*
import cats.syntax.functor.*
import util.homogeneoustuple.{ Triple, given }

object Pe001:
  val sumOfMultiples = (n: Int) => Reader((limit: Long) =>
    val l = (limit - 1) / n
    (1 + l) * l / 2 * n
  )
  // def calc: Reader[Long, Long] = (3, 5, 15).map(sumOfMultiples).mapN(_ + _ - _)
  def solve(limit: Long) = (3, 5, 15).map(sumOfMultiples).mapN(_ + _ - _).run(limit)

  @main def main01 = run(solve(1000))
