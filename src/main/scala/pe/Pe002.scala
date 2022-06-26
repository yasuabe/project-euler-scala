package pe

object Pe002:
  def solve(limit: Int) = LazyList
    .iterate((1, 2, 0L))((a, b, s) => (b, a + b, s + (if b % 2 == 0 then b else 0)))
    .dropWhile(x => x._2 < limit)
    .head._3

  @main def main002 = run(solve(4_000_000))
