package pe

import munit.FunSuite

class Pe001Suite extends FunSuite:
  test("solve 001 correctly") { assertEquals(Pe001.solve, 233168L) }
