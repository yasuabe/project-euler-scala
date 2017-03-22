package pe

object Pe016 {
  def solve(p: Int) = BigInt(2).pow(p).toString.toCharArray.map(_.toInt - '0').sum

  def main(args: Array[String]): Unit = run(solve(1000))
}
