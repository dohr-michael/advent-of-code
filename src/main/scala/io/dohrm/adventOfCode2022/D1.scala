package io.dohrm.adventOfCode2022

object D1 extends Base {
  override def sample: String =
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

  final case class Bag(items: List[Int] = Nil) {
    def add(item: Int): Bag = copy(items = items :+ item)
    def total: Int = items.sum
  }

  def part1(bags: List[Bag]): Unit =
    println(
      bags
        .map(_.total)
        .max
    )

  def part2(bags: List[Bag]): Unit =
    println(
      bags
        .map(_.total)
        .sorted
        .reverse
        .take(3)
        .sum
    )

  def readAll(value: String): List[Bag] =
    value.split("\n").map(_.trim).foldLeft(List(Bag())) { (acc, c) =>
      if (c == "") {
        Bag() +: acc
      } else {
        acc.head.add(c.toInt) +: acc.tail
      }
    }
  def main(args: Array[String]): Unit =
    part2(readAll(read))

}
