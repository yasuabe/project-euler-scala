package pe

import scala.annotation.tailrec

object Pe036 {
  def isPalindrome(n: Int): Boolean = n == reverse(n, 0)

  @tailrec def loop(x: Int, t: Stream[Int]): Stream[Int] =
    if (x == 0) t else loop(x >> 1, t.map(y => (y << 1) + (x & 1)))

  def makePalindromePair(n: Int) = loop(n, Stream(n >> 1, n))

  @tailrec def reverse(n: Int, r: Int): Int = if (n == 0) r else reverse(n / 10, r * 10 + n % 10)

  def solve(limit: Int): Int = Stream
    .from(1)
    .flatMap(makePalindromePair)
    .takeWhile(_ < limit)
    .filter(isPalindrome)
    .sum

  def main(args: Array[String]): Unit = run(solve(1000000))
}
