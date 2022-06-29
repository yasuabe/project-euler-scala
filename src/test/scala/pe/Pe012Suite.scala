package pe

import munit.FunSuite

class Pe012Suite extends FunSuite:
  test("solve 012 example"  ) { assertEquals(Pe012.solve(    5).get,       28L) }
  test("solve 012 correctly") { assertEquals(Pe012.solve(  500).get, 76576500L) }
  test("solve 012 with memo") { assertEquals(Pe012_2.solve         , 76576500L) }
