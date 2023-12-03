package io.dohrm.adventOfCode.y2023

class D3Spec extends munit.FunSuite {
  val sample1: String =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  test("part1") {
    assertEquals(D3.part1(sample1), 4361)
  }

  test("part2") {
    assertEquals(D3.part2(sample1), 467835)
  }
}
