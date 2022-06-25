package pe

object Pe031 {
  def solve(limit: Int) = {
    val arr = Array.fill(limit + 1)(1)
    def g(p: Int) = (p to limit).foreach(i => arr(i) += arr(i - p))

    List(2, 5, 10, 20, 50, 100, 200).foreach(g)
    arr(limit)
  }
  def main(args: Array[String]): Unit = run(solve(200))
}
