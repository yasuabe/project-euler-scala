package pe

object Pe030 {
  val fifthPows: Seq[Long] = (0 to 9).map(pow(_, 5))

  def f(n: Long, sum: Long): Long = if (n == 0) sum else {
    val (d, m) = divMod(n, 10)
    f(d, sum + fifthPows(m.toInt))
  }
  def solve(p: Int): Int = (2 to 999999).filter(n => n == f(n, 0)).sum

  def main(args: Array[String]): Unit = run(solve(5))
}
