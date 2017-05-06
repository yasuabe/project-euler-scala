package pe

object Pe045 extends BaseApp[Long] {
  import pe.Pe044.isPenta
  def solve(n: Long): Long = {
    val h = n*(2*n - 1)
    if (isPenta(h)) h else solve(n + 1)
  }
  def main: Long = solve(144)
}
