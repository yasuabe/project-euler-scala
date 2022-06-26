package pe

import munit.FunSuite

class Pe004Suite extends FunSuite:
  test("solve 004 sample"   ) { assertEquals(Pe004.solve( 99),   9009) }
  test("solve 004 correctly") { assertEquals(Pe004.solve(999), 906609) }