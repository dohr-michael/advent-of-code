package io.dohrm.adventOfCode.y2023
import scala.collection.mutable
import scala.util.matching.Regex

object D1 extends Base {

  private val digits = (0 until 10).map(c => c.toString.r -> c).toList
  private val letters = List(
    "one".r -> 1,
    "two".r -> 2,
    "three".r -> 3,
    "four".r -> 4,
    "five".r -> 5,
    "six".r -> 6,
    "seven".r -> 7,
    "eight".r -> 8,
    "nine".r -> 9
  )
  def part1(value: String): Int = read(value, digits).sum
  def part2(value: String): Int = read(value, digits ++ letters).sum
  private def read(value: String, values: List[(Regex, Int)]): List[Int] = {
    value
      .split("\n")
      .map { row =>
        val results = values
          .flatMap(v => v._1.findAllMatchIn(row).map(v._2 -> _.start))
          .sortBy(_._2)
          .map(_._1)
        if (results.length < 1) 0
        else results.head * 10 + results.last
      }
      .toList
  }

  def main(args: Array[String]): Unit =
    println(part1(read))
    println(part2(read))
}
