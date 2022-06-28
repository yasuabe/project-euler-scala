package pe

import mouse.boolean.*
import mouse.int.*
import Math.sqrt

object Pe009:
  def product(s: Int, m: Int, k: Int) =
    val d = s / (2 * m * k)
    val n = k - m
    val a = d * (m.squared - n.squared)
    val b = 2 * d * m * n
    val c = d * (m.squared + n.squared)
    a * b * c

  def solve(s :Int): Int =
    def remove2s(n: Int): Int = n.isEven.fold(remove2s(n / 2), n)
    val s2 = s / 2

    (2 to sqrt(s2).toInt)
      .toList
      .filter(_ divides s2)
      .flatMap { m =>
        val sm = remove2s(s2 / m)
        LazyList
          .from     (m + m.isOdd.fold(2, 1), 2)
          .takeWhile(k => k < 2 * m && k <= sm)
          .find     (k => (k divides sm) && (k coprime m))
          .map      (product(s, m, _))
      }.head

  @main def main009 = run(solve(1000))
