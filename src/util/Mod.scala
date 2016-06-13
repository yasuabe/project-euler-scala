package util

case class Mod(modulus: Long) {
  def add(a: Long, b: Long): Long =
    ((a % this.modulus) + (b % this.modulus)) % this.modulus

  def multiply(a: Long, b: Long): Long =
    (a % modulus) * (b % modulus) % modulus

  def pow(r: Long, e: Long): Long =
    if      (e == 0) 1
    else if ((e & 1) == 1) multiply(r, pow(multiply(r, r), e >> 1))
    else pow(multiply(r, r), e >> 1)

  def sum(ms: Long*): Long = ms.map(m => m % modulus).foldLeft(0L)(add(_, _))

}
