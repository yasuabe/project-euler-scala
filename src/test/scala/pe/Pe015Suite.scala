package pe

import munit.FunSuite

class Pe015Suite extends FunSuite:
  test("solve 015 example"  ) { assertEquals(Pe015.solve( 2),            6L) }
  test("solve 015 correctly") { assertEquals(Pe015.solve(20), 137846528820L) }
