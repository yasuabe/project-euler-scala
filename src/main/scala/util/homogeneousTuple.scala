package util

import cats.Functor

object homogeneoustuple:
  
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
