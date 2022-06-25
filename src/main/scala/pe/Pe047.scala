package pe

import util.Primes

import scala.collection.SortedSet

// 無限ストリーム版
object Pe047 extends BaseApp[Long] {
  sealed trait Elem {
    val product: Long
    def children: List[Elem]
  }
  case class Elem1(ps: List[Long], primes: Stream[Long]) extends Elem {
    override val product: Long = ps.product
    override def children: List[Elem] = {
      def loop(ps1: List[Long] = ps :+ primes.head, ps2: List[Long] = Nil): List[List[Long]] = ps1 match {
        case _ :: Nil => Nil
        case h :: t   => ps2.foldRight(t)(_ :: _) :: loop(t, h :: ps2)
      }
      loop().map(Elem1(_, primes.tail)) ++ Elem2(product, ps.reverse).children
    }
  }
  case class Elem2(product: Long, ns: List[Long]) extends Elem {
    override def children: List[Elem] =
      ns.tails.toList.reverse.tail.map(l => Elem2(product * l.head, l))
  }
  implicit val o: Ordering[Elem] = new Ordering[Elem] {
    override def compare(x: Elem, y: Elem): Int = x.product compare y.product
  }
  def unfold(ss: SortedSet[Elem]): Stream[Long] = {
    val h = ss.head
    val newSS = h.children.foldLeft(ss.tail)(_ + _)
    h.product #:: unfold(newSS)
  }
  def solve(n: Int) = {
    def findSolution(ms: Stream[Long]): Long = {
      val firstN = ms.take(n)
      if (firstN.last - firstN.head == n - 1) firstN.head else findSolution(ms.tail)
    }
    val (ps, primes)      = Primes.primes().splitAt(n)
    val initialElem: Elem = Elem1(ps.toList, primes)
    val numbers           = unfold(SortedSet(initialElem))

    findSolution(numbers)
  }
  def main: Long = solve(4)
}
