package util

object  Utils:
  def run[T](f: => T): Unit =
    val start = System.currentTimeMillis()
    printf("Ans. %s (%,d ms)%n", f, System.currentTimeMillis() - start)

