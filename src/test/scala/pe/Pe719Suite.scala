package pe

import munit.FunSuite

class Pe719Suite extends FunSuite:
  test("solve 719 small sample") { assertEquals(Pe719.solve(    100L),           41333L) }
  test("solve 719 correctly"   ) { assertEquals(Pe719.solve(1000000L), 128088830547982L) }
