package pe

import pe.lists._
import scala.util.Right

object Pe044 extends BaseApp[Long] {
  // P(n+k) - P(n) = (6n + 3n^2 - n) / 2
  // P(n+k) + P(n) = (6n^2 + 6nk + 3k^2 - 2n - k) / 2

  case class Param(k: Int, step: Int, n: Int, diff: Long)

  def isPenta(x: Double): Boolean       = isInt((math.sqrt(24*x + 1) + 1) / 6.0)
  def isPenta(n: Long, k: Int): Boolean = isPenta((6*n*n + 6*n*k + 3*k*k - 2*n - k) / 2.0)

  def firstParam(n: Int): Param = Param(n, 3*n, 1, (6*n + 3*n*n - n) / 2)

  def searchToLimit(limit: Long, p: Param): Either[Param, Long] = {
    def go(n: Int, diff: Long): Either[Param, Long] =
      if      (diff >= limit)                    Left(p.copy(n = n, diff = diff))
      else if (isPenta(diff) && isPenta(n, p.k)) Right(diff)
      else                                       go(n + 1, diff + p.step)
    go(p.n, p.diff)
  }
  def searchForK(k: Int, ps: List[Param]): Either[List[Param], Long] = {
    val next = firstParam(k + 1)
    ps.map(searchToLimit(next.diff, _)).findRight.left.map(next :: _)
  }
  def solve(k: Int, l: List[Param]): Long =
    searchForK(k, l).fold(solve(k + 1, _), n => n)

  def main: Long = solve(1, List(firstParam(1)))
}
