import scala.annotation.tailrec

def divMod10(n: Long, m: Int) = (n / m, (n % m).toInt)

def f(target: Long, ds: List[Int]): Boolean = ds match 
  case d1 :: d2 :: dt  => f(target, d1 * 10 + d2 :: dt) || f(target - d1, d2 :: dt)
  case d  :: Nil       => target == d

f( 9, List(8, 1))
f( 6, List(3, 6))
f(99, List(9, 8, 0, 1))
f(91, List(8, 2, 8, 1))
f(82, List(6, 7, 2, 4))

def g(n: Long): List[Int] =
  @tailrec def h(m: Long, acc: List[Int]): List[Int] =
    if m == 0 then acc else 
      val (d, r) = divMod10(m, 10)
      h(d, r :: acc)
  h(n * n, List.empty[Int])
  
g( 9)
g(91)
g(99)

def judge(n: Long) = f(n, g(n))
judge(99)
judge(98)
judge(91)

(1L to 100L).filter(judge).map(x => x.toLong * x)
(2L to 100L).filter(judge).map(x => x.toLong * x).sum
(1 to 100).filter(judge).sum
(1L to 1000000L).filter(judge).map(x => x.toLong * x).sum