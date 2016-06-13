package pe

import util.Utils.run
import scala.util.Sorting

object Pe461 {
  val N: Int = 10000
  val L: Int = (math.log(math.Pi + 1) * N).toInt

  def main(args: Array[String]) = run(()=>solve)

  def solve: Long = {
    val fs   = initFs()
    val sums = initSums(fs)

    val (low, high) = fromBothEnds(sums, Math.PI)
    val (fa, fb)    = fromBothEnds(fs, sums(low))
    val (fc, fd)    = fromBothEnds(fs, sums(high))

    Seq(fa, fb, fc, fd).map(e => e * e).sum
  }

  def initFs() = (0 to L).map(i => math.exp(i / N.toDouble) - 1).toArray

  def initSums(fs: Array[Double]): Array[Double] = {
    val result:Array[Double] = new Array[Double](L * (L - 1) / 2)
    def f(i: Int, j: Int, k: Int): Int = if (j < i) {
      result(k) = fs(i) + fs(j)
      f(i, j + 1, k + 1)
    } else k
    (2 to L).foldLeft(0)((k, i) => f(i, 1, k))
    Sorting.quickSort(result)
    result
  }

  def fromBothEnds(arr: Array[Double], criteria: Double): (Int, Int) = {
    def f(curL: Int, curU: Int, resL: Int, resU: Int, diff: Double): (Int, Int) = if (curL < curU) {
      val diffCand = math.abs(criteria - (arr(curL) + arr(curU)))
      val (diff2, resL2, resU2) = if (diffCand < diff) (diffCand, curL, curU)
                                  else                 (diff, resL, resU)
      if      (criteria < arr(curL) + arr(curU)) f(curL, curU - 1, resL2, resU2, diff2)
      else if (criteria > arr(curL) + arr(curU)) f(curL + 1, curU, resL2, resU2, diff2)
      else (resL2, resU2)
    } else (resL, resU)
    return f(0, lowerBound(arr, criteria), 0, 0, Double.MaxValue)
  }

  def lowerBound(arr: Array[Double], value: Double): Int = {
    def f(low: Int, high: Int): Int = if (low < high) {
      val mid = (low + high) / 2
      if (arr(mid) <= value) f(mid + 1, high)
      else f(low, mid)
    } else low - 1
    f(0, arr.length)
  }
}