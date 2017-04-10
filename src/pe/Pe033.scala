package pe

/*
 * 1 <= a < c <= 9
 * (10a+b) / (10b+c) = a / c
 * 9ac / (10a-c) = b <= 9
 */
object Pe033 extends UseFraction {
  def solve = (1 to 8).flatMap { a =>
    (a + 1 to 9).filter { c =>
      val d = 9 * a * c / (10 * a - c).toDouble
      d <= 9 && d == d.toInt
    }.map(frac(a, _))
  }.foldLeft(frac(1, 1))(multiply)
  ._2

  def main(args: Array[String]): Unit = run(solve)
}
