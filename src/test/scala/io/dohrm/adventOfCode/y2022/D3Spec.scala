package io.dohrm.adventOfCode.y2022

class D3Spec extends munit.FunSuite {
  val sample: String =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  test("part1") {
    assertEquals(D3.part1(sample), 157)
  }

  test("part2") {
    assertEquals(D3.part2(sample), 70)
  }

}
