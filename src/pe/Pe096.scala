package pe

import util.Utils._
import scala.collection.immutable.Queue

case class Board(cs: Seq[Set[Int]], q: Queue[Int]) {
  def enqueue(pos: Int) =
      Board(cs, if (q.contains(pos)) q else q.enqueue(pos))
  def dequeue() = q.dequeue match { case (e, es) => (e, Board(cs, es)) }

  def update(pos: Int, possibles: Set[Int]): Board =
      Board(cs.updated(pos, possibles), q)

  def determine(pos: Int, n: Int) = update(pos, Set(n)).enqueue(pos)
  def cancel(pos: Int, n: Int) = {
    val newSet = cs(pos) - n
    (cs(pos).contains(n), newSet.size) match {
      case (_,    0) => None
      case (true, 1) => Some(update(pos, newSet).enqueue(pos))
      case _         => Some(update(pos, newSet))
    }
  }
  def foldAll[A, B](ps: Seq[A], maybeB: Option[B])(f: (B, A) => Option[B]): Option[B] = maybeB match {
    case None    => None
    case Some(b) => ps match {
      case Seq()            => Some(b)
      case Seq(pos, ps2@_*) => foldAll(ps2, f(b, pos))(f)
    }
  }
  def cancelAll(poss: Seq[Int], n: Int) =
      foldAll(poss, Some(this))((b, pos) => b.cancel(pos, n))

  def deduce: Option[Board] = if (q.isEmpty) Some(this) else {
    val (pos, b) = dequeue()
    b.cancelAll(Board.mates(pos).toSeq, cs(pos).last) match {
      case None     => None
      case Some(b2) => b2.deduce
    }
  }
  def firstUndetermined(b: Board) =
    b.cs.zipWithIndex.filter(_._1.size > 1).sortBy(_._1.size).headOption

  def proceed: Option[Int] = {
    deduce match {
      case None    => None
      case Some(b) => firstUndetermined(b) match {
        case None           => Some(b.leftTopValue)
        case Some((arr, y)) => arr.flatMap(z => b.determine(y, z).proceed).headOption
      }
    }
  }
  def leftTopValue = Seq(0, 1, 2).foldLeft(0)((acc, n) => acc*10 + cs(n).head)
}
object Board {
  def e9 = Array.fill[Set[Int]](9)(Set.empty)
  def rowColBox(p: Int) = (p / 9, p % 9, p / 27 * 3 + p / 3 % 3)
  val (rows, cols, boxes) = (0 to 80).foldLeft((e9, e9, e9)) { case ((rs, cs, bs), p) =>
    val (r, c, b) = rowColBox(p)
    (rs.updated(r, rs(r) + p), cs.updated(c, cs(c) + p), bs.updated(b, bs(b) + p))
  }
  def mates(p: Int): Set[Int] = {
    val (r, c, b) = rowColBox(p)
    (rows(r) ++ cols(c) ++ boxes(b)) - p
  }
  def apply(ns: Seq[Int]): Board = {
    val (cells, queue) = ns.zipWithIndex
      .foldRight((List.empty: List[Set[Int]], Queue.empty: Queue[Int])) {
        case ((n, pos), (s, q)) => n match {
          case 0 => ((1 to 9).toSet :: s, q)
          case _ => (Set(n) :: s, q.enqueue(pos))
        }
    }
    Board(cells, queue)
  }
}
object Pe096 {
  def main(args: Array[String]) = run(() => solveAll)
  def solveAll = loadQuestions.map(solveOne(_)).sum
  def solveOne(initialNumbers: Seq[Int]) = Board(initialNumbers).proceed.get

  def loadQuestions = parseLines(loadLines, Seq.empty)
  def parseLines(lines: Seq[String], result: Seq[Seq[Int]]): Seq[Seq[Int]] = lines match {
    case Seq()         => result
    case Seq(_, xs@_*) =>
      val (first9, rest) = xs.splitAt(9)
      parseLines(rest, result :+ toIntSeq(first9))
  }
  def toIntSeq(lines: Seq[String]): Seq[Int] =
      lines.mkString("").toCharArray.map(_.-('0'))
  def loadLines = io.Source
    .fromInputStream(getClass.getResourceAsStream("/resources/sudoku.txt"))
    .mkString.lines.toSeq
}
