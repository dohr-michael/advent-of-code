package io.dohrm.adventOfCode.y2023

import scala.collection.mutable

object D6 extends Base {
  case class Race(time: Long, distance: Long) {
    // ax² + bx + c = 0
    // x1 = (-b + (b²-4ac)^1/2)/2a
    // x2 = (-b - (b²-4ac)^1/2)/2a

    // -x² + time * x - distance > 0
    // a = -1
    // b = time
    // c = -distance
    def countWayToWin: Long = {
      val base = Math.pow(Math.pow(time.toDouble, 2) - 4d * -1d * -distance.toDouble, 0.5)
      val x1 = (-time.toDouble + base) / 2d * -1d
      val x2 = (-time.toDouble - base) / 2d * -1d
      val min = Math.min(x1, x2)
      val max = Math.max(x1, x2)

      // Exclusive
      val maxB = if (max.toLong.toDouble == max) max.toLong - 1 else max.toLong
      maxB - min.toLong
    }
  }

  def parse(input: String): List[Race] = {
    val inputs = input
      .split("\n")
      .map(_.split(":").last.trim.split(" ").filter(_.nonEmpty).map(_.trim.toInt))
    inputs.head.zip(inputs.last).map(c => Race(c._1, c._2)).toList
  }

  def part1(value: String): Long = {
    val races = parse(value)

    races.map(_.countWayToWin).product
  }

  def part2(value: String): Long = {
    val races = parse(value)
    val tmp = races.foldLeft(("", "")) { (acc, c) => (acc._1 + c.time.toString, acc._2 + c.distance.toString) }
    val race = Race(tmp._1.toLong, tmp._2.toLong)
    race.countWayToWin
  }

  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }

}
