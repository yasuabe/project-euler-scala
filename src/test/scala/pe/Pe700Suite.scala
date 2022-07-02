package pe

import munit.FunSuite

class Pe700Suite extends FunSuite:
  test("solve 700 small sample") { assertEquals(Pe700.solve(              43L,               20L),               78L) }
  test("solve 700 correctly"   ) { assertEquals(Pe700.solve(4503599627370517L, 1504170715041707L), 1517926517777556L) }
