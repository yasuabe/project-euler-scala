package pe

import munit.FunSuite

class Pe007Suite extends FunSuite:
  test("solve 007 example"  ) { assertEquals(Pe007.solve(    6),     13L) }
  test("solve 007 correctly") { assertEquals(Pe007.solve(10001), 104743L) }