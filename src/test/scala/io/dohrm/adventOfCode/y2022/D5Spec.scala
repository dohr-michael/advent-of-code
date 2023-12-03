package io.dohrm.adventOfCode.y2022

class D5Spec extends munit.FunSuite {
  val sample: String =
    """    [D]    
      |[N] [C]    
      |[Z] [M] [P]
      | 1   2   3 
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin

  test("part1") {
    assertEquals(D5.part1(sample), "CMZ")
  }

  test("part2") {
    assertEquals(D5.part2(sample), "MCD")
  }

}
