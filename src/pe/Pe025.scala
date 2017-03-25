package pe

import java.lang.Math.log10

object Pe025 {
  val sqrt5       = Math.sqrt(5)
  val logTen      = Math.log(10)
  val goldenRatio = (1.0 + Math.sqrt(5.0)) / 2.0

  // log_10 { φ^n / √5 } = n*log_10(φ) - log_10(√5)
  def digits(n: Int): Int = (n * log10(goldenRatio) - log10(sqrt5)).toInt + 1

  def solve(target: Int) = {
    def searchMax(n: Int): Int = if (digits(n) > target) n else searchMax(n << 1)

    def matches(n: Int): Int =
      if      (digits(n - 1) > target - 1)  1
      else if (digits(n)     < target    ) -1
      else                                  0

    def loop(lo: Int, hi: Int): Int = {
      if      (matches(lo) == 0) lo
      else if (matches(hi) == 0) hi
      else {
        val half = (lo + hi) / 2
        matches(half) match {
          case  0 => half
          case -1 => loop(half + 1, hi   - 1)
          case  1 => loop(lo   + 1, half - 1)
        }
      }
    }
    loop(1, searchMax(1))
  }
  def main(args: Array[String]): Unit = run(solve(1000))
}
