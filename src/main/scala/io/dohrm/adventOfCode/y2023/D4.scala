package io.dohrm.adventOfCode.y2023

object D4 extends Base {
  case class Card(
      id: Int,
      winning: List[Int],
      played: List[Int]
  ) {
    lazy val valid: List[Int] = played.filter(v => winning.contains(v))
    lazy val score: Int =
      if (valid.isEmpty) {
        0
      } else {
        valid.tail.foldLeft(1) { (acc, _) => acc * 2 }
      }
  }

  def parseNumbers(value: String): List[Int] =
    value
      .split(' ')
      .filter(_.nonEmpty)
      .map(_.trim.toInt)
      .toList
  def parse(value: String): List[Card] =
    value
      .split("\n")
      .map { row =>
        val input = row.split(":")
        val elements = input.last.split('|')
        Card(
          input.head.stripPrefix("Card ").trim.toInt,
          parseNumbers(elements.head),
          parseNumbers(elements.last)
        )
      }
      .toList
  def part1(value: String): Int =
    parse(value).map(_.score).sum
  def part2(value: String): Int = {
    val results = parse(value)
    val first0 = results.indexWhere(_.valid.isEmpty)
    val realItems = results
    realItems
      .foldLeft(realItems.map(_.id -> 1).toMap) { (acc, c) =>
        (c.id + 1)
          .until(Math.min(c.id + 1 + c.valid.length, results.length))
          .foldLeft(acc) { (acc2, d) =>
            acc2 + (d -> (acc2.getOrElse(d, 1) + acc.getOrElse(c.id, 0)))
          }
      }
      .values
      .sum
  }
  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }

}
