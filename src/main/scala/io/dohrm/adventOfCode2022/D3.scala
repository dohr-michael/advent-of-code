package io.dohrm.adventOfCode2022

object D3 extends Base {
  override def sample: String =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  def getPriority(c: Char): Int =
    if (c <= 'z' && c >= 'a') c.toInt + 1 - 'a'.toInt
    else if (c <= 'Z' && c >= 'A') c.toInt + 1 - 'A'.toInt + 26
    else 0
  final case class Rucksack(first: Array[(Char, Int)], second: Array[(Char, Int)]) {
    lazy val allItems: Array[(Char, Int)] = (first ++ second).sortBy(_._1)
    lazy val findItem: Option[(Char, Int)] = first.find(v => second.exists(_._1 == v._1))
  }

  def rucksack(list: String): Rucksack = Rucksack(
    list.substring(0, list.length / 2).toCharArray.map(c => c -> getPriority(c)).sortBy(_._1),
    list.substring(list.length / 2).toCharArray.map(c => c -> getPriority(c)).sortBy(_._1)
  )

  def part1(items: List[Rucksack]): Unit =
    println(items.flatMap(_.findItem.toList).map(_._2).sum)

  def part2(items: List[Rucksack]): Unit = {
    println(
      items
        .grouped(3)
        .flatMap { items =>
          items
            .flatMap(v => v.allItems.distinct)
            .groupBy(v => v)
            .find(_._2.size >= 3)
            .map(_._1._2)
            .toList
        }
        .sum
    )
  }
  def run(fn: List[Rucksack] => Unit, value: String): Unit =
    fn(
      value.split("\n").map(_.trim).map(rucksack).toList
    )

  def main(args: Array[String]): Unit =
    run(
      part2,
      read
    )

}
