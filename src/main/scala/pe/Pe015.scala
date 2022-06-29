package pe

object Pe015 {
  type Fraction = (Long, Long)
  def frac(n: Long, d: Long): Fraction = {
    val g = n.gcd(d)
    (n / g, d / g)
  }
  def multiply(f1: Fraction, f2: Fraction): Fraction =
    frac(f1._1 * f2._1, f1._2 * f2._2)

  def solve = {
    val fh :: ft = (1L to 20L).map(d => frac(d + 20, d)).toList
    ft.foldLeft(fh)((acc, e) => multiply(acc, e))
  }
  def main(args: Array[String]): Unit = run(solve._1)
}
