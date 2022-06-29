package pe

import munit.FunSuite

class Pe014Suite extends FunSuite:
  test("solve 014 small example") { assertEquals(Pe014.solve(       10)._1,      9) }
  test("solve 014 correctly"    ) { assertEquals(Pe014.solve(1_000_000)._1, 837799) }
