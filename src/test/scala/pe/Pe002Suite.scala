package pe

import munit.FunSuite

class Pe002Suite extends FunSuite:
  test("solve 002 sample"   ) { assertEquals(Pe002.solve(100)      , 44L     ) }
  test("solve 002 correctly") { assertEquals(Pe002.solve(4_000_000), 4613732L) }
