package pe

import munit.FunSuite
import cats.syntax.option.*

class Pe011Suite extends FunSuite:
  test("solve 011 correctly") { assertEquals(Pe011.solve, 70600674L.some) }
