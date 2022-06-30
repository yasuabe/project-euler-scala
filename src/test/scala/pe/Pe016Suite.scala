package pe

import munit.FunSuite

class Pe016Suite extends FunSuite:
  test("solve 016 example"  ) { assertEquals(Pe016.solve(  15),   26) }
  test("solve 016 correctly") { assertEquals(Pe016.solve(1000), 1366) }
