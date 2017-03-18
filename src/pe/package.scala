import util.Utils

package object pe {
  def run[T](f: => T) = Utils.run(f)
}
