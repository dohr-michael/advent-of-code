package io.dohrm.adventOfCode.y2023

class D6Spec extends munit.FunSuite {
  val sample: String =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  test("part1") {
    assertEquals(D6.part1(sample), 288L)
  }

  test("part2") {
    assertEquals(D6.part2(sample), 71503L)
  }
}
