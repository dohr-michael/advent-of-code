package io.dohrm.adventOfCode.y2023

class D8Spec extends munit.FunSuite {
  val sample: String =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  test("part1") {
    assertEquals(D8.part1(sample), 1)
  }

  test("part2") {
    assertEquals(D8.part2(sample), 0)
  }
}
