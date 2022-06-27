package pe

import munit.FunSuite

class Pe008Suite extends FunSuite:
  test("solve 008 example"  ) { assertEquals(Pe008.solve( 4),        5832L) }
  test("solve 008 correctly") { assertEquals(Pe008.solve(13), 23514624000L) }