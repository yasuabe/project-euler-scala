package pe

object Pe601 {
  def main(args: Array[String]) = run(solve(31))

  def solve(lim: Long): Long = {
    def lcm(a: Long, b: Long) = a * b / a.gcd(b)
    def go(i: Int, n0: Long, p0: Long, acc: Long): Long = {
      if (i > lim) acc else {
        val n = n0 - 1
        val p = lcm(p0, i)
        val q = lcm(p, i + 1)
        val P = n / p - n / q

        go(i + 1, n0 * 4, p, P + acc)
      }
    }
    go(1 + 1, 4 * 4, 1, 1)
  }
}