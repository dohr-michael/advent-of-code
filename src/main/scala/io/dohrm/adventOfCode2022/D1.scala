package io.dohrm.adventOfCode2022

object D1 extends Base {

  final case class Bag(items: List[Int] = Nil) {
    def add(item: Int): Bag = copy(items = items :+ item)
    def total: Int = items.sum
  }

  def part1(value: String): Int =
    readAll(value)
      .map(_.total)
      .max

  def part2(value: String): Int =
    readAll(value)
      .map(_.total)
      .sorted
      .reverse
      .take(3)
      .sum

  def readAll(value: String): List[Bag] =
    value.split("\n").map(_.trim).foldLeft(List(Bag())) { (acc, c) =>
      if (c == "") {
        Bag() +: acc
      } else {
        acc.head.add(c.toInt) +: acc.tail
      }
    }
  def main(args: Array[String]): Unit =
    println(part2(read))

}
