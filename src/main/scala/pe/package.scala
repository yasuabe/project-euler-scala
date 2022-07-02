import cats.Show
import cats.Applicative
import cats.syntax.show.*
import util.{Lists, Utils}
import mouse.boolean.*

import scala.language.higherKinds

package object pe {
  extension (n: Int)
    infix def gcd(b: Int): Int = (b == 0).fold(n, (b gcd (n % b)))
    infix def divides(m: Int): Boolean = m % n == 0
    infix def coprime(m: Int): Boolean = (n gcd m) == 1
    infix def isEven: Boolean = n % 2 == 0
    infix def isOdd: Boolean = !n.isEven

  extension (n: Long)
    infix def gcd(b: Long): Long = (b == 0).fold(n, (b gcd (n % b)))
    infix def divides(m: Long): Boolean = m % n == 0
    infix def coprime(m: Long): Boolean = (n gcd m) == 1
    infix def isEven: Boolean = n % 2 == 0
    infix def isOdd: Boolean = !n.isEven
    inline def sqr: Long = n * n

  extension [T : Show] (s: T)
    def toInts: List[Int] = s.show.map(_ - '0').toList

  def run[T](f: => T) = Utils.run(f)
  def divMod[A](n: A, m: A)(implicit integral: Integral[A]): (A, A) = {
    import integral._
    (n / m, n % m)
  }
  def loadResource(name: String): String =
    scala.io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/resources/$name")).mkString

  def isInt(n: Double): Boolean = n.toInt == n
  trait UseFraction {
    type Fraction = (Long, Long)
    def frac(n: Long, d: Long): Fraction = {
      val g = n.gcd(d)
      (n / g, d / g)
    }
    def multiply(f1: Fraction, f2: Fraction): Fraction =
      frac(f1._1 * f2._1, f1._2 * f2._2)
  }
  def divMod2(n: Int): (Int, Int) = (n >>> 1, n & 1)
  def pow(n: Long, p: Int): Long = divMod2(p) match {
    case (0, 0) => 1
    case (d, 0) => sqr(pow(n, d))
    case (d, 1) => sqr(pow(n, d)) * n
  }
  def sqrt(n: Int): Int = Math.sqrt(n).toInt
  def on[F[_], S, T, U](a: (S, S))(f: S => F[T])(g: (T, T) => U)(implicit F: Applicative[F]): F[U] =
    F.map2(f(a._1), f(a._2))(g)

  abstract class BaseApp[A] {
    def main: A
    def main(args: Array[String]): Unit = run(main)
  }
  trait UsesWords {
    val fileName: String
    def words: List[String] =
      loadResource(fileName).tail.reverse.tail.reverse.split("""","""").toList
  }
  implicit class TupleOps[A](val t: (A, A)) {
    def mapT[B](f: A => B): (B, B) = (f(t._1), f(t._2))
  }
  def plusMinus(d: Double): (Double, Double) = (d, -d)
  def solution(a: Double, b: Double, c: Double): Option[(Double, Double)] =
    Some(b*b - 4*a*c)
      .filter(_ >= 0)
      .map(d => plusMinus(Math.sqrt(d)).mapT(D2 => (-b + D2) / (2*a)))

  val lists = Lists
}
