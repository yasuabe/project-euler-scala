package util

import scala.util.Right

object Lists {
  implicit class ListEitherOps[L, R](val l: List[Either[L, R]]) extends AnyVal {
    def findRight: Either[List[L], R] = l.find(_.isRight) match {
      case Some(Right(r)) => Right(r)
      case _              => Left(l.map(_.left.get))
    }
  }
}
