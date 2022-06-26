package util

object LazyPrimes:
  // TODO: refactoring
  import PQueue._

  sealed trait PQueue:
    def merge(another: PQueue): PQueue

  case object PNil extends PQueue:
    def merge(another: PQueue): PQueue = another

  case class PCons(ns: LazyList[Long], qs: List[PQueue]) extends PQueue:
    def join(q2: PQueue): PQueue = PCons(ns, q2 :: qs)
    def merge(y: PQueue): PQueue = y match {
      case PNil      => this
      case y1: PCons => if (ns.head <= y1.ns.head) join(y1) else y1.join(this)
    }

  object PQueue:
    def enqueue(ns: LazyList[Long], q: PQueue): PQueue = PCons(ns, Nil).merge(q)
    def mergeAll(qs: List[PQueue]): PQueue = qs match {
      case Nil            => PNil
      case q :: Nil       => q
      case q1 :: q2 :: qs => q1.merge(q2).merge(mergeAll(qs))
    }

  type Wheel = LazyList[Long]

  def cycle(ns: LazyList[Long]): LazyList[Long] = ns #::: cycle(ns)
  def spin(start: Long, wheel: Wheel): Wheel = cycle(wheel).scan(start)(_ + _)

  def comps(ns: LazyList[Long]): LazyList[Long] = comps(ns.head, ns)
  def comps(m: Long, ns: LazyList[Long]): LazyList[Long] = ns match
    case n #:: ns => (m * n) #:: comps(m, ns)

  def sieve(nns: LazyList[Long], q: PQueue): LazyList[Long] = (nns, q) match
    case (n #:: ns, PNil)                => n #:: sieve(ns, enqueue(comps(nns), PNil))
    case (n #:: ns, PCons(m #:: ms, qs)) =>
      if (n < m) n #:: sieve(ns, enqueue(comps(nns), q))
      else       sieve(if (m == n) ns else nns, enqueue(ms, mergeAll(qs)))
    case (_, _)                          => ???

  def next(ps: LazyList[Long], xs: LazyList[Long]): (LazyList[Long], LazyList[Long]) = ps match
    case p #:: _ =>
      val (y #:: ys) = cycle(xs)
      val py = p + y
      (py #:: ps, cancel(ps.product, p, py, ys))

  def cancel(m: Long, p: Long, n: Long, xs: LazyList[Long]): LazyList[Long] = (m, xs) match
    case (0, _)        => LazyList.empty[Long]
    case (_, x #:: ys) =>
      val nx = n + x
      if ((nx % p) > 0) x #:: cancel(m - x, p, nx, ys)
      else cancel(m, p, n, (x + ys.head) #:: ys.tail)

  def wheel(n: Int): (LazyList[Long], LazyList[Long]) =
    List.iterate((LazyList(2L), LazyList(1L)), n + 1)((next _).tupled).last

  def primes(n: Int = 6): LazyList[Long] = wheel(n) match
    case (p #:: ps, ns) => sieve(ps.reverse #::: spin(p, ns), PNil)

  def intLazyPrimes = LazyPrimes.primes().map(_.toInt)

  def primeFactors(n: Long, primes: => LazyList[Long] = LazyPrimes.primes()): List[(Long, Int)] =
    def go(n: Long, ps: LazyList[Long], result: List[(Long, Int)]): List[(Long, Int)] =
      if (n == 1) result
      else ps match { case ph #:: pt =>
        if (n % ph == 0) result match {
          case (p, c)  :: rt if p == ph => go(n / ph, ps, (ph, c + 1) :: rt)
          case _ => go(n / ph, ps, (ph, 1) :: result)
        } else if (n < ph * ph) (n, 1) :: result
        else go(n, pt, result)
      }
    go(n, primes, Nil)
