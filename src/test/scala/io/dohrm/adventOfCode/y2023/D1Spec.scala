package io.dohrm.adventOfCode.y2023

class D1Spec extends munit.FunSuite {
  val sample1: String =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  val sample2: String =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin

  test("part1") {
    assertEquals(D1.part1(sample1), 142)
  }

  test("part2") {
    assertEquals(D1.part2(sample2), 281)
  }
}
