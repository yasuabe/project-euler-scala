package pe

import cats.{Applicative, Id}
import cats.data.{Reader, ReaderT}
import cats.instances.list._

import scala.collection.mutable

object Pe039_Reader extends BaseApp[Int] {
  type Limit[A] = Reader[Int, A]

  def perimeter(m: Int, n: Int): Int = 2*m*(m + n)

  def limit: Limit[Int] = ReaderT.ask[Id, Int]
  def maxM:  Limit[Int] = limit.map(l => solution(1, 1, -l / 2).get._1.toInt)

  def primeFactorSet: Limit[List[(Int, List[Int])]] = maxM.map { max =>
    val arr = Array.fill[List[Int]](max + 1)(Nil)
    (2 to max / 2).foreach { i =>
      if (arr(i).isEmpty) (2*i to (max, i)).foreach(j => arr.update(j, i :: arr(j)))
    }
    arr.toList.zipWithIndex.map(_.swap).drop(2)
  }
  def coprimePairs(t: (Int, List[Int])): Limit[List[Int]] = limit.map { l =>
    val (m, ps) = t
    val upper   = (m - 1) min (l/2/m - m)
    val bs      = mutable.BitSet(1 to upper: _*)
    ps.foreach(p => (p until (m, p)).foreach(bs -= _))
    bs.toList
  }
  def primitivePerimeters(t: (Int, List[Int])): Limit[List[Int]] =
    coprimePairs(t).map(_.map(perimeter(t._1, _)))

  def perimeters(pfs: List[(Int, List[Int])]): Limit[List[Int]] =
    Applicative[Limit].sequence(pfs map primitivePerimeters).map(_.flatten)

  def counts(perimeters: List[Int]): Limit[List[(Int, Int)]] = Reader { limit =>
    val arr = Array.fill(limit + 1)(0)
    perimeters.foreach(p => (p to (limit, p)).foreach(arr(_) += 1))
    arr.zipWithIndex.toList
  }
  def solve: Limit[Int] = for {
    mns <- primeFactorSet
    ps  <- perimeters(mns)
    cs  <- counts(ps)
  } yield cs.sortBy(_._1).reverse.head._2

  def main: Int = solve.run(1000)
}
