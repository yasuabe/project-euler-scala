package pe

import munit.FunSuite

class Pe009Suite extends FunSuite:
  test("solve 009 example"  ) { assertEquals(Pe009.solve(  12),       60) }
  test("solve 009 correctly") { assertEquals(Pe009.solve(1000), 31875000) }
