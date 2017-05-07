package pe

import util.{Heap, PairingHeap}
import util.Collections.unfold

object Pe046 extends BaseApp[Long] {
  import PairingHeap._
  type Primes = Stream[Long]
  type Params = (Long, Int, Primes)
  def paramsAt3(n: Int): Params = (fomula(3, n), n, primesFrom5)
  implicit val o: Ordering[Params] = new Ordering[Params] {
    def compare(x: Params, y: Params): Int = x._1 compare y._1
  }
  def fomula(p: Long, n: Int): Long = p + 2 * n * n

  def proceed(params: PairingHeap[Params]): (Long, PairingHeap[Params]) = {
    val Heap((g, _, _), _) = params
    def go(cur: PairingHeap[Params]): PairingHeap[Params] = pop(cur) match {
      case ((g2, n, ps@(p #:: pt)), next) if g == g2 =>
        val y = insert((fomula(p, n), n, pt), next)
        val z = if (ps == primesFrom5) insert(paramsAt3(n + 1), y) else y
        go(z)
      case _ => cur
    }
    (g, go(params))
  }
  def search(os: Stream[Long], ps: Primes, gs: Stream[Long]): Long = (os, ps, gs) match {
    case (o #:: ot, p #:: pt, g #:: gt) =>
      if      (o <  p && o <  g)  o
      else if (o == p && o == g) search(ot, pt, gt)
      else if (o == p)           search(ot, pt, gs)
      else if (o == g)           search(ot, ps, gt)
      else sys.error("should not be reached")
  }
  // 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29...
  val oddStream          = Stream.iterate(5L)(_ + 2)
  // 5, 7,    11, 13,     17, 19,     23,         29...
  val p3 #:: primesFrom5 = util.Primes.primes().tail
  // 5, 7, 9, 11, 13, 15,     19, 21, 23, 25, 27, 29, 31...
  def goldbachStream     = unfold(PairingHeap(paramsAt3(1)))(proceed)

  def main: Long = search(oddStream, primesFrom5, goldbachStream)
}
