package pe

import scala.annotation.tailrec

object Pe719:
  def toList(n: Long): List[Int] =
    @tailrec def h(m: Long, acc: List[Int]): List[Int] =
      if m == 0 then acc else h(m / 10, (m % 10).toInt :: acc)
    h(n.sqr, Nil)
    
  def judge(n: Long): Boolean =
    def f(target: Long, ds: List[Int]): Boolean = ds match 
      case d1 :: d2 :: dt => f(target, d1 * 10 + d2 :: dt) || f(target - d1, d2 :: dt)
      case d  :: Nil      => target == d
    val r9 = n % 9
    (r9 == 0 || r9 == 1) && f(n, toList(n))

  def solve(n: Long): Long = (2L to n).filter(judge).map(_.sqr).sum

  @main def main719 = run(solve(1000000L))
