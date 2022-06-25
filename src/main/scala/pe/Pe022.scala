package pe

import util.PairingHeap

object Pe022 extends BaseApp[Long] with UsesWords {
  override val fileName: String = "p022_names.txt"

  def value(name: String): Long = name.toCharArray.map(_ - 'A' + 1).sum

  def solve(names: List[String]): Long = {
    import PairingHeap._

    def go(pos: Int, ph: PairingHeap[String], result: Long): Long = ph match {
      case util.Empty => result
      case util.Heap(elem, subHeaps) =>
        go(pos + 1, mergePairs(subHeaps), pos * value(elem) + result)
    }
    val namesHeap = names.foldRight[PairingHeap[String]](util.Empty)(insert)

    go(1, namesHeap, 0)
  }
  def namesIn(text: String): List[String] =
    text.split(",").map(s => s.substring(1, s.length - 1)).toList

  def main: Long = solve(words)
}
