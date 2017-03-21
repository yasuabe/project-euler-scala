package pe

object Pe067 {
  def main(args: Array[String]): Unit = run {
    Pe018.solve(loadResource("p067_triangle.txt").lines.toList)
  }
}
