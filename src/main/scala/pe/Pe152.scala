package pe

import util.Primes.primes
import cats.syntax.compose._
import cats.instances.function._

object Pe152 {
  val lim = 80
  type Frac = (Long, Long)

  def plus(f1: Frac, f2: Frac): Frac = {
    val n = f1._1 * f2._2 + f2._1 * f1._2
    val d = f1._2 * f2._2
    val cd = n.gcd(d)
    (n / cd, d / cd)
  }
  def patternsOf7k: List[Frac] = {
    def loop(k: Int, ks: List[Int], sum: Frac): List[List[Int]] => List[List[Int]] =
      acc => {
        val m = k + 1
        if (k >= 80 / 7)
          if (sum._2 % 7 != 0) ks :: acc else acc
        else
          (loop(m, ks, sum) <<< loop(m, m :: ks, plus(sum, (1, m * m * 49))))(acc)
      }
    loop(0, Nil, (0, 1))(Nil)
      .filter(l => !(l.contains(7) || l.contains(11)))
      .map(_.map(n => (1L, 49L * n * n)).fold((0L, 1L))(plus))
  }
  def compositsOf235 = {
    val bs = scala.collection.mutable.BitSet(2 to lim: _*)
    primes()
      .dropWhile(_ < 7)
      .takeWhile(_ <= lim)
      .foreach(p => (p to (lim, p)).foreach(l => bs.remove(l.toInt)))
    bs.toList.map(_.toLong)
  }
  def availableNumbers = compositsOf235
                        .diff(List(16, 32, 48, 64))
                        .diff(List(25, 50, 75))
                        .diff(List(27, 54))
                        .map(p => (1L, p * p))
                        .+:((1L, 144L)) // k = 1, 3, 4 for 1 / (k * 13)^2

  def count(prev: Frac, ns: List[Frac], rs: List[Frac]): Int => Int =
    cnt => ns match {
      case Nil    => cnt
      case n :: t =>
        def compare(f1: Frac, f2: Frac) = f1._1 * f2._2 - f2._1 * f1._2
        if (compare(plus(prev, rs.head), (1, 2)) < 0) cnt
        else {
          val ord = compare((1, 2), plus(prev, n))
          val f   = (f: Frac) => count(f, t, rs.tail)

          if      (ord < 0) f(prev)(cnt)
          else if (ord > 0) (f(prev) <<< f(plus(prev, n)))(cnt)
          else              f(prev)(cnt + 1)
        }
    }
  def sumOfRests = availableNumbers.scanRight((0L, 1L))(plus).init

  def solve: Int =
    patternsOf7k.map(count(_, availableNumbers, sumOfRests)(0)).sum

  def main(args: Array[String]): Unit = run(solve)
}
