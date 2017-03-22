package pe

object Pe017 {
  def lengths(s: String*) = s.map(_.length)
  val digit1 = lengths(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine")
  val digits10to19 = lengths(
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen")
  val digit2 = lengths(
    "twenty",
    "thirty",
    "fourty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety")
  val digit3 = lengths(
    "hundred",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen")
  val len100  = "hundred".length
  val lenAND  = "and".length
  val len1000 = ("one" + "thousand").length

  def solve = {
    val ls1_9   = digit1.sum
    val ls10_19 = digits10to19.sum
    val ls20_99 = digit2.map(_ * 10 + ls1_9).sum
    val ls1_99  = ls1_9 + ls10_19 + ls20_99
    ls1_99 * 10 + lenAND * 99 * 9 + (ls1_9 + len100 * 9) * 100 + len1000
  }
  def main(args: Array[String]): Unit = run(solve)
}
