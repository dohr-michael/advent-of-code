package io.dohrm.adventOfCode.y2023

class D9Spec extends munit.FunSuite {
  val sample: String =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  test("part1") {
    assertEquals(D9.part1(sample), 114L)
  }

  test("part2") {
    assertEquals(D9.part2(sample), 2L)
  }
}
