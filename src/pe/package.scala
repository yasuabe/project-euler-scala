import util.Utils

package object pe {
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def run[T](f: => T) = Utils.run(f)
  def divMod(n: Long, m: Long): (Long, Long) = (n / m, n % m)
  def loadResource(name: String): String =
    io.Source.fromInputStream(getClass.getResourceAsStream(s"/resources/$name")).mkString

  trait UseFraction {
    type Fraction = (Long, Long)
    def frac(n: Long, d: Long): Fraction = {
      val g = gcd(n, d)
      (n / g, d / g)
    }
    def multiply(f1: Fraction, f2: Fraction): Fraction =
      frac(f1._1 * f2._1, f1._2 * f2._2)
  }
  def divMod2(n: Int): (Int, Int) = (n >>> 1, n & 1)
  def sqr(n: Long): Long = n * n
  def pow(n: Long, p: Int): Long = divMod2(p) match {
    case (0, 0) => 1
    case (d, 0) => sqr(pow(n, d))
    case (d, 1) => sqr(pow(n, d)) * n
  }

}
