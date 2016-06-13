package pe

object Pe506 {
  val L = 100000000000000L
  val Q = L / 15
  val R = (L % 15).toInt
  val M = 123454321

  val As = Array(0,1,2,3,4,32,123,43,2123,432,1234,32123,43212,34321,23432)
  val Bs = Array(123432,234321,343212,432123,321234,123432,432123,212343,432123,123432,321234,432123,343212,234321,123432)
  val SumA = As.sum
  val SumB = Bs.sum
  val T = 1000000L

  val R1 = solveEq(T - 1)
  val R2 = solveEq(sq(T - 1))

  def mult(a: Long, b: Long): Long = a * b % M
  def sq(a:Long): Long = mult(a, a)
  def pow(r: Long, e: Long): Long = e match {
    case 0 => 1
    case _ => mult(sq(pow(r, e / 2)), if (e % 2 == 0) 1 else r)
  }
  def sumToR(a: Array[Int]): Long = (0 to R).map(a(_)).sum

  def solveEq(x: Long): Long = {
    def f(a: Long, b: Long, c0: Long, c1: Long): Long = {
      a % b match {
        case 1 => c0 - (a / b) * c1
        case _ => f(b, a % b, c1, c0 - (a / b) * c1)
      }
    }
    f(x, M % x, 1, -(M / x))
  }
  def f1: Long = mult(mult(pow(T, Q) - 1, SumA), R1)
  def f2: Long = mult(mult(mult(1 - Q, T - 1) + mult(T, pow(T, Q - 1) - 1), SumB), R2)

  def f3: Long = mult(pow(T, Q),          sumToR(As))
  def f4: Long = mult(mult(pow(T, Q) - 1, sumToR(Bs)), R1)

  def solve: Long = {
    val r = (f1 + f2 + f3 + f4) % M
    if (r < 0) r + M else r
  }
  def main(args: Array[String]) {
    val start = System.currentTimeMillis()
    printf("Ans:%d (%dms)", solve, System.currentTimeMillis() - start)
  }
}
