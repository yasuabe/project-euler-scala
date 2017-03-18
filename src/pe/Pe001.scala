package pe

import cats.data.Reader

object Pe001 {
  def sumOfMultiples(n: Int): Reader[Long, Long] = Reader { (limit: Long) =>
    val l = (limit - 1) / n
    (1 + l) * l / 2 * n
  }
  def calc: Reader[Long, Long] = {
    for {
      m3  <- sumOfMultiples(3)
      m5  <- sumOfMultiples(5)
      m15 <- sumOfMultiples(15)
    } yield m3 + m5 - m15
  }
  def main(args: Array[String]) = run(calc.run(1000))
}
