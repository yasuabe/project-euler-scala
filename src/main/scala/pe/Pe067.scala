package pe

import scala.collection.immutable.StringOps
object Pe067 {
  def main(args: Array[String]): Unit = run {
    Pe018.solve(StringOps(loadResource("p067_triangle.txt")).lines.toList)
  }
}
