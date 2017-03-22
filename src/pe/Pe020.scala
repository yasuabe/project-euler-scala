package pe

object Pe020 {
  def solve = (1 to 100).map(BigInt.apply).product.toString.map(_.toInt - '0').sum
  def main(args: Array[String]): Unit = run(solve)
}
