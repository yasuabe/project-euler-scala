package pe

object Pe009 {
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def makeProduct(s: Int, m: Int, k: Int) = {
    val d = s / (2 * m * k)
    val n = k - m
    val a = d * (m * m - n * n)
    val b = 2 * d * m * n
    val c = d * (m * m + n * n)
    a * b * c
  }
  def solve(s :Int): Option[Int] = {
    val s2 = s / 2
    (2 until Math.ceil(Math.sqrt(s2)).toInt)
      .toStream
      .filter(m => s2 % m == 0)
      .flatMap { m =>
        val sm = removeTwos(s2 / m)
        Stream
          .from     (m + (if (m % 2 == 1) 2 else 1), 2)
          .takeWhile(k => k < 2 * m && k <= sm)
          .find     (k => sm % k == 0 && gcd(k, m) == 1)
          .map      (makeProduct(s, m, _))
      }.headOption
  }
  def removeTwos(n: Int): Int = n % 2 match {
    case 0 => removeTwos(n / 2)
    case _ => n
  }
  def main(args: Array[String]) = run(solve(1000))

}
