package pe

object Pe042 extends BaseApp[Int] with UsesWords {
  override val fileName: String = "p042_words.txt"

  def wordValueOf(s: String): Int = s.toCharArray.map(_ - 'A' + 1).sum

  def prepareTriangleNumbers(max: Int): Set[Int] = Stream
    .iterate((1, 1)) { case (n, t) => (n + 1, n * (n + 1) / 2) }
    .map(_._2)
    .takeWhile(_ <= max)
    .toSet

  def solve = {
    val vs = words.map(wordValueOf)
    val ts = prepareTriangleNumbers(vs.max)
    vs.count(ts.contains)
  }
  def main: Int = solve
}
