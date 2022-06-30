package pe

object Pe016:
  def solve(p: Int): Int = BigInt(2).pow(p).toInts.sum

  @main def main016: Unit = run(solve(1000))
