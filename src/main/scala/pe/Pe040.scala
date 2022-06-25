package pe

object Pe040 extends BaseApp[Int] {
  def pow10(n: Int) = pow(10, n).toInt
  def d(n: Int, min: Int, m: Int, w: Int): Int = {
    val (d, r) = divMod(n - min - 1, w)
    (d + 1 + m).toString.charAt(r) - '0'
  }
  def solve(limit: Int) = {
    def f(n: Int, m: Int, w: Int, min: Int, max: Int, ans: Int): Int =
      if (n > limit) ans
      else if (max < n)
        f(n,    10*m + 9, w+1, max, max + 9 * pow10(w) * (w+1), ans)
      else
        f(10*n, m,        w,   min, max,                        ans * d(n, min, m, w))
    f(n = 1, m = 0, w = 1, min = 0, max = 9, ans = 1)
  }
  def main: Int = solve(1000000)
}
