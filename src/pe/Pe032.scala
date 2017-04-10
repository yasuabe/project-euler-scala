package pe

object Pe032 {
  val digits = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

  def toInt(ds: List[Int]): Int = ds.foldLeft(0)(_ * 10 + _)
  def triplet(ds: List[Int], t: (Int, Int)): Option[Int] = {
    val (a, as) = ds.splitAt(t._1)
    val (b, c)  = as.splitAt(t._2)
    Some(toInt(c)).filter(_ == toInt(a) * toInt(b))
  }
  def solve(pairs: (Int, Int)*) = digits
    .permutations
    .flatMap(ds => pairs.flatMap(p => triplet(ds, p)))
    .toSet
    .sum

  def main(args: Array[String]): Unit = run(solve((1, 3), (1, 4), (2, 2), (2, 3)))
}
