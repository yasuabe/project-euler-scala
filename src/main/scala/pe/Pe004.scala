package pe

import scala.annotation.tailrec

object Pe004 {
  def isPalindrome(n: Int): Boolean    = isPalindrome(n.toString)
  def isPalindrome(s: String): Boolean = s == s.reverse

  @tailrec
  def solve(max: Int, row: Int, col: Int): Int = {
    if (row * row <= max) return max

    val prod = row * col

    if (prod <= max)             solve(max,  row - 1)
    else if (isPalindrome(prod)) solve(prod, row - 1)
    else                         solve(max,  row, col - 1)
  }
  def solve(max: Int, n: Int): Int = solve(max, n, n)
  def solve(n: Int): Int           = solve(0, n)

  def main(args: Array[String]) = run(solve(999))
}
