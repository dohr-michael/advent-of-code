package io.dohrm.adventOfCode2022

class D4Spec extends munit.FunSuite {
  val sample: String =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin

  test("part1") {
    assertEquals(D4.part1(sample), 2)
  }

  test("part2") {
    assertEquals(D4.part2(sample), 4)
  }

}
