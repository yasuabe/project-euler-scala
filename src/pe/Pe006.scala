package pe

object Pe006 {
  // Σ(k^2)   = n(n+1)(2n+1)/6 ... ①
  // (Σ(k))^2 = n^2(n+1)^2/4   ... ②
  // ① - ②   = (3n^3 + 2n^2 - 3n - 2) * n / 12
  def solve(n: Int) =
    Seq(3, 2, -3, -2, 0).foldLeft(0)((acc, c) => acc * n + c) / 12

  def main(args: Array[String]) = run(solve(100))
}
