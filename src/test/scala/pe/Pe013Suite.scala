package pe

import munit.FunSuite

class Pe013Suite extends FunSuite:
  test("solve 013 correctly") { assertEquals(Pe013.solve, 5537376230L.toString) }
