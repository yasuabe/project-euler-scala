package pe

import util.Mod
import util.Utils.run

object Pe463 {
  val L: Long = 450283905890997363L
  val N: Long = L / 4
  val M:  Mod = Mod(1000000000L)
  val M3: Mod = Mod(1000000000L * 3)

  def main(args: Array[String]) = run(()=>solve)

  def solve: Long = {
    val n    = (math.log(N) / math.log(2)).toLong
    val p2   = 1L << n
    val rest = N - (p2 - 1)

    val sum1 = M3.multiply(M3.pow(4, n), 16) / 3
    val sum2 = f(rest, p2 << 1, 1, 0)
    val sum3 = M.multiply(12 * (p2 - 1) + 16, rest)

    M.sum(sum1, sum2, sum3)
  }
  def f(rest: Long, p2: Long, d2: Long, acc: Long): Long =
    if (rest < d2) acc
    else {
      val d1 = d2 * 4
      val (q1, r1) = divMod(rest, d1)
      val (q2, r2) = divMod(r1, d2)

      val d2q1 = M.multiply(d2, q1)
      val sum2 = 2 * (d2q1 + (if (q2 == 1) r2 else if (q2 > 1) d2 else 0))
      val sum1 = 1 * (d2q1 + (if (q2 == 2) r2 else if (q2 > 2) d2 else 0))
      val sum3 = 3 * (d2q1 + (if (q2 == 3) r2 else 0))

      f(rest, p2 / 4, d2*4, acc + M.multiply(p2, sum2 + sum1 + sum3))
    }
  def divMod(n: Long, m: Long): (Long, Long) = (n / m, n % m)
}