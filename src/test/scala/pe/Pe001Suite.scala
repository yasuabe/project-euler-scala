package pe

import munit.FunSuite

class Pe001Suite extends FunSuite:
  test("solve 001 sample"   ) { assertEquals(Pe001.solve(10)  , 23L    ) }
  test("solve 001 correctly") { assertEquals(Pe001.solve(1000), 233168L) }
