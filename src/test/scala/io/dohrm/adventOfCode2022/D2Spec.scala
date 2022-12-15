package io.dohrm.adventOfCode2022

class D2Spec extends munit.FunSuite {
  val sample: String =
    """A Y
      |B X
      |C Z""".stripMargin

  test("part1") {
    assertEquals(D2.part1(sample), 15)
  }

  test("part2") {
    assertEquals(D2.part2(sample), 12)
  }

}
