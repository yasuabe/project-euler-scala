package pe

object Pe043 extends BaseApp[Long] {
  import List.empty
  def setBit(n: Int, at: Int): Int = n | (1 << at)
  def toDecimal(ds: List[Int]): Long = ds.foldLeft(0L)(_ * 10 + _)

  def iterateMod[A](m: Int, rep: Int)(n: Int, res: A, d: Int = 0)(f: (Int, A, Int) => A): A =
    if (d == rep) res else iterateMod(m, rep)(n / m, f(n, res, d), d + 1)(f)

  def toBits(n: Int): Int           = iterateMod(10, 3)(n, 0)         ((a, b, _) => setBit(b, a % 10))
  def to3Digits(n: Int): List[Int]  = iterateMod(10, 3)(n, empty[Int])((m, r, _) => m % 10 :: r)
  def iterate10[A](n: Int, res: A, d: Int = 0)(f: (A, Int) => A, g: (A, Int) => A): A =
    iterateMod(2, 10)(n, res)((a, b, c) => (if((a & 1) == 1) f else g)(b, c))

  def countBits(n: Int): Int        = iterate10(n, 0)         ((r, _) => r + 1, (r, _) => r)
  def openDigits(m: Int): List[Int] = iterate10(m, empty[Int])((r, _) => r,     (r, d) => d :: r)

  def bitsDigitsPair(n: Int): Option[(Int, List[Int])] =
    Some(toBits(n), to3Digits(n)).filter(t => countBits(t._1) == 3)

  def pan17 = (17 to (1000, 17)).toList.flatMap(bitsDigitsPair)
  def panN(n: Int): (Int, List[Int]) => List[(Int, List[Int])] = (bs: Int, ds: List[Int]) => {
    openDigits(bs).flatMap(d =>
      if (toDecimal(d :: ds.take(2)) % n == 0) Some(setBit(bs, d), d :: ds) else None
    )
  }
  def solve = List(13, 11, 7, 5, 3, 2)
      .foldLeft(pan17)((acc, p) => acc.flatMap(panN(p).tupled))
      .map { case (b, ds) => toDecimal(openDigits(b).head :: ds) }
      .sum

  def main: Long = solve
}
