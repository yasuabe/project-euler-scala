package pe

import munit.FunSuite

class Pe006Suite extends FunSuite:
  test("solve 006 example"   ) { assertEquals(Pe006.solve( 10),     2640) }
  test("solve 006 correctly")  { assertEquals(Pe006.solve(100), 25164150) }