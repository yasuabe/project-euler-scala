package pe

import munit.FunSuite

class Pe003Suite extends FunSuite:
  test("solve 003 sample"   ) { assertEquals(Pe003.solve(       13195L),   29L) }
  test("solve 003 correctly") { assertEquals(Pe003.solve(600851475143L), 6857L) }