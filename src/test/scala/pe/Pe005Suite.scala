package pe

import munit.FunSuite

class Pe005Suite extends FunSuite:
  test("solve 005 sample"   ) { assertEquals(Pe005.solve(10),      2520L) }
  test("solve 005 correctly") { assertEquals(Pe005.solve(20), 232792560L) }