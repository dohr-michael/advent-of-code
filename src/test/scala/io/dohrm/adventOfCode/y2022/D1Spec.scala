package io.dohrm.adventOfCode.y2022

class D1Spec extends munit.FunSuite {
  val sample: String =
    """1000
      |2000
      |3000
      |
      |4000
      |
      |5000
      |6000
      |
      |7000
      |8000
      |9000
      |
      |10000""".stripMargin

  test("part1") {
    assertEquals(D1.part1(sample), 24000)
  }

  test("part2") {
    assertEquals(D1.part2(sample), 45000)
  }

}
