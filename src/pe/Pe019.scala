package pe

object Pe019 {
  def daysInFeburary(year: Int): Int =
    (year % 400, year % 100, year % 4) match {
      case (0, _, _) => 29
      case (_, 0, _) => 28
      case (_, _, 0) => 29
      case _ => 28
    }
  def daysInMonth(y: Int, m: Int): Int =
    if      (m == 2)                                daysInFeburary(y)
    else if (m == 4 || m == 6 || m == 9 || m == 11) 30
    else                                            31

  def solve = Stream.iterate((0, 1)) { case (m, w) =>
    val (year, month) = (m / 12 + 1900, m % 12 + 1)
    (m + 1, (w + daysInMonth(month, month)) % 7)
  } .slice(12, 12 * 101)
    .count(t => t._2 == 0)

  def main(args: Array[String]): Unit = run(solve)
}
