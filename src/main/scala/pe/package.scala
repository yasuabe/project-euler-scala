import cats.Applicative
import util.{Lists, Utils}

import scala.language.higherKinds

package object pe {
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def run[T](f: => T) = Utils.run(f)
  def divMod[A](n: A, m: A)(implicit integral: Integral[A]): (A, A) = {
    import integral._
    (n / m, n % m)
  }
  def loadResource(name: String): String =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(s"/resources/$name")).mkString

  def isInt(n: Double): Boolean = n.toInt == n
  trait UseFraction {
    type Fraction = (Long, Long)
    def frac(n: Long, d: Long): Fraction = {
      val g = gcd(n, d)
      (n / g, d / g)
    }
    def multiply(f1: Fraction, f2: Fraction): Fraction =
      frac(f1._1 * f2._1, f1._2 * f2._2)
  }
  def divMod2(n: Int): (Int, Int) = (n >>> 1, n & 1)
  def sqr(n: Long): Long = n * n
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
