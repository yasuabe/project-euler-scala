package pe

import scala.collection.immutable.::

object Pe024 {
  // TODO: 汎用性高い
  def removeAt(l: List[Int], n: Int): (Int, List[Int]) = {
    val (hs, h :: ht) = l.splitAt(n)
    (h, hs ++ ht)
  }
  def phase(m: Int, n: Int): (Int, Int) = {
    val mm = m - 1
    (mm / n, mm % n + 1)
  }
  def solve(i: Int) = {
    val h :: t = (1 to 9)
      .scan(1)(_ * _)
      .reverse
      .toList
    t.scanLeft(phase(i, h)) { case ((_, m), n) => phase(m, n) }
      .map(_._1)
      .foldLeft((List.empty[Int], (0 to 9).toList)) { case ((la, lb), pos) =>
        val (v, ld) = removeAt(lb, pos)
        (v :: la, ld)
      }._1
      .reverse
      .foldLeft(0L)((acc, n) => acc * 10L + n)
  }
  def main(args: Array[String]): Unit = run(solve(1000000))
}
