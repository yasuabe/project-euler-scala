package util

sealed trait PQueue {
  def merge(another: PQueue): PQueue
}
case object PNil extends PQueue {
  def merge(another: PQueue): PQueue = another
}
case class PCons(ns: Stream[Long], qs: List[PQueue]) extends PQueue {
  def join(q2: PQueue): PQueue = PCons(ns, q2 :: qs)
  def merge(y: PQueue): PQueue = y match {
    case PNil      => this
    case y1: PCons => if (ns.head <= y1.ns.head) join(y1) else y1.join(this)
  }
}
object PQueue {
  def enqueue(ns: Stream[Long], q: PQueue): PQueue = PCons(ns, Nil).merge(q)
  def mergeAll(qs: List[PQueue]): PQueue = qs match {
    case Nil            => PNil
    case q :: Nil       => q
    case q1 :: q2 :: qs => q1.merge(q2).merge(mergeAll(qs))
  }
}

object Primes {
  import PQueue._

  type Wheel = Stream[Long]

  def cycle(ns: Stream[Long]): Stream[Long] = ns.append(cycle(ns))
  def spin(start: Long, wheel: Wheel): Wheel = cycle(wheel).scan(start)(_ + _)

  def comps(ns: Stream[Long]): Stream[Long] = comps(ns.head, ns)
  def comps(m: Long, ns: Stream[Long]): Stream[Long] = ns match {
    case n #:: ns => (m * n) #:: comps(m, ns)
  }
  def sieve(nns: Stream[Long], q: PQueue): Stream[Long] = (nns, q) match {
    case (n #:: ns, PNil) => n #:: sieve(ns, enqueue(comps(nns), PNil))
    case (n #:: ns, PCons(m #:: ms, qs)) =>
      if (n < m) n #:: sieve(ns, enqueue(comps(nns), q))
      else       sieve(if (m == n) ns else nns, enqueue(ms, mergeAll(qs)))
  }
  def next(ps: Stream[Long], xs: Stream[Long]): (Stream[Long], Stream[Long]) = ps match {
    case p #:: _ =>
      val (y #:: ys) = cycle(xs)
      val py = p + y
      (py #:: ps, cancel(ps.product, p, py, ys))
  }
  def cancel(m: Long, p: Long, n: Long, xs: Stream[Long]): Stream[Long] = (m, xs) match {
    case (0, _)        => Stream.empty[Long]
    case (_, x #:: ys) =>
      val nx = n + x
      if ((nx % p) > 0) x #:: cancel(m - x, p, nx, ys)
      else cancel(m, p, n, (x + ys.head) #:: ys.tail)
  }
  def wheel(n: Int): (Stream[Long], Stream[Long]) =
    List.iterate((Stream(2L), Stream(1L)), n + 1)((next _).tupled).last

  def primes(n: Int = 6): Stream[Long] = wheel(n) match {
    case (p #:: ps, ns) => sieve(ps.reverse.append(spin(p, ns)), PNil)
  }
}
