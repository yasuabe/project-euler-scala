import util.Utils

package object pe {
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def run[T](f: => T) = Utils.run(f)
  def divMod(n: Long, m: Long): (Long, Long) = (n / m, n % m)
  def loadResource(name: String): String =
    io.Source.fromInputStream(getClass.getResourceAsStream(s"/resources/$name")).mkString
}
