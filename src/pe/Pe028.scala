package pe

object Pe028 {
  def solve(n: Long): Long = 4 * n * (4 * n - 2) + 4
  def main(args: Array[String]): Unit = run((1L to 500L).map(solve).sum + 1)
}
