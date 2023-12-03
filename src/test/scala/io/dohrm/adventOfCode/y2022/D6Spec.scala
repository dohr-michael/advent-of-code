package io.dohrm.adventOfCode.y2022

class D6Spec extends munit.FunSuite {
  val samples: List[(String, Int, Int)] = List(
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
    ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
  )

  test("part1") {
    samples.foreach { c =>
      assertEquals(D6.part1(c._1), c._2)
    }
  }

  test("part2") {
    samples.foreach { c =>
      assertEquals(D6.part2(c._1), c._3)
    }
  }

}
