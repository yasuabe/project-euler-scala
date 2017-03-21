import util.Utils

package object pe {
  def run[T](f: => T) = Utils.run(f)
  def divMod(n: Long, m: Long): (Long, Long) = (n / m, n % m)
  def loadResource(name: String): String =
    io.Source.fromInputStream(getClass.getResourceAsStream(s"/resources/$name")).mkString
}
