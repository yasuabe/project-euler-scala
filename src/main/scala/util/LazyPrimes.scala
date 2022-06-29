package util

import mouse.boolean.*

object LazyPrimes:
  // TODO: refactoring
  import PQueue._

  enum PQueue:
    case PCons(ns: LazyList[Long], qs: List[PQueue])
    case PNil

  object PQueue:
    extension (q: PCons)
      infix def join(q2: PQueue): PQueue = PCons(q.ns, q2 :: q.qs)

    extension (q: PQueue)
      def merge(another: PQueue): PQueue = q match
        case PNil             => another
        case q@ PCons(ns, qs) => another match
          case PNil     => q
          case y: PCons => if ns.head <= y.ns.head then q join y else y join q

    def enqueue(ns: LazyList[Long], q: PQueue): PQueue = PCons(ns, Nil).merge(q)
    def mergeAll(qs: List[PQueue]): PQueue = qs match
      case Nil            => PNil
      case q :: Nil       => q
      case q1 :: q2 :: qs => q1.merge(q2).merge(mergeAll(qs))

  type Wheel = LazyList[Long]

  def cycle(ns: Wheel): Wheel                = ns #::: cycle(ns)
  def spin(start: Long, wheel: Wheel): Wheel = cycle(wheel).scan(start)(_ + _)

  def comps(ns: LazyList[Long]): LazyList[Long] = comps(ns.head, ns)
  def comps(m: Long, ns: LazyList[Long]): LazyList[Long] = ns match
    case n #:: ns => (m * n) #:: comps(m, ns)

  def sieve(nns: LazyList[Long], q: PQueue): LazyList[Long] = (nns, q) match
    case (n #:: ns, PNil)                => n #:: sieve(ns, enqueue(comps(nns), PNil))
    case (n #:: ns, PCons(m #:: ms, qs)) =>
      if n < m
      then n #:: sieve(ns, enqueue(comps(nns), q))
      else sieve((m == n).fold(ns, nns), enqueue(ms, mergeAll(qs)))
    case (_, _)                          => ???

  def next(ps: LazyList[Long], xs: LazyList[Long]): (LazyList[Long], LazyList[Long]) =
    val p #:: _  = ps
    val y #:: ys = cycle(xs)
    val py       = p + y
    (py #:: ps, cancel(ps.product, p, py, ys))

  def cancel(m: Long, p: Long, n: Long, xs: LazyList[Long]): LazyList[Long] = (m, xs) match
    case (0, _)        => LazyList.empty[Long]
    case (_, x #:: ys) =>
      val nx = n + x
      if (nx % p) > 0
      then x #:: cancel(m - x, p, nx, ys)
      else cancel(m, p, n, (x + ys.head) #:: ys.tail)

  def wheel(n: Int): (LazyList[Long], LazyList[Long]) =
    List.iterate((LazyList(2L), LazyList(1L)), n + 1)((next _).tupled).last

  def primes(n: Int = 6): LazyList[Long] = wheel(n) match
    case (p #:: ps, ns) => sieve(ps.reverse #::: spin(p, ns), PNil)

  def intLazyPrimes = LazyPrimes.primes().map(_.toInt)

  def primeFactors(n: Long, primes: => LazyList[Long] = LazyPrimes.primes()): List[(Long, Int)] =
    def go(n: Long, ps: LazyList[Long], result: List[(Long, Int)]): List[(Long, Int)] =
      if n == 1 then result
      else
        val ph #:: pt = ps
        if n % ph == 0 then
          result match
            case (p, c)  :: rt if p == ph => go(n / ph, ps, (ph, c + 1) :: rt)
            case _                        => go(n / ph, ps, (ph, 1) :: result)
        else if (n < ph * ph) then (n, 1) :: result
        else                       go(n, pt, result)

    go(n, primes, Nil)
