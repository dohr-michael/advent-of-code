package io.dohrm.adventOfCode.y2023

class D8Spec extends munit.FunSuite {
  val sample: String =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  test("part1") {
    assertEquals(D8.part1(sample), 6L)
  }

  test("part2") {
    assertEquals(D8.part2("""LR
                            |
                            |11A = (11B, XXX)
                            |11B = (XXX, 11Z)
                            |11Z = (11B, XXX)
                            |22A = (22B, XXX)
                            |22B = (22C, 22C)
                            |22C = (22Z, 22Z)
                            |22Z = (22B, 22B)
                            |XXX = (XXX, XXX)""".stripMargin), 6L)
  }
}
