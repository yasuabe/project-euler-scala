package pe

import munit.FunSuite

class Pe010Suite extends FunSuite:
  test("solve 010 example"  ) { assertEquals(Pe010.solve(       10),           17L) }
  test("solve 010 correctly") { assertEquals(Pe010.solve(2_000_000), 142913828922L) }
