package util

import cats.Monoid
import cats.Applicative
import cats.Functor
import cats.syntax.arrow.*

object homogeneoustuple:
  type Pair[T] = (T, T)

  given applicativePair: Applicative[Pair] with
    def pure[T](a: T): Pair[T] = (a, a)
    def ap[A, B](ff: Pair[A => B])(fa: Pair[A]): Pair[B] =
      (ff._1 split ff._2)(fa)

  given Monoid[Pair[Int]] with
    def empty: Pair[Int] = applicativePair.pure(0)
    def combine(a: Pair[Int], b: Pair[Int]): Pair[Int] =
      (a._1 + b._1, a._2 + b._2)

  def toPolyFunc[B, R](g: B => R): [T] => T => R = [t] => (_: t) match
    case b: B @unchecked => g(b)
  
  type Triple[T] = (T, T, T)

  given tuple3Functor: Functor[Triple] with
    def map[A, B](from: (A, A, A))(g: A => B): (B, B, B) = from.map[[T] =>> B](toPolyFunc(g))

  given tuple3FunctorOpsConversion: Conversion[Triple[Int], Functor.Ops[Triple, Int]] with
    def apply(t: Triple[Int]): Functor.Ops[Triple, Int] = new Functor.Ops {
      type TypeClassType = Functor[Triple]
      def self = t
      val typeClassInstance = tuple3Functor
    }
