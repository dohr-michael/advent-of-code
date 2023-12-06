package io.dohrm.adventOfCode.y2023

import scala.collection.mutable

object D6 extends Base {
  case class Race(time: Long, distance: Long) {
    def wayToWin: List[Long] = {
      var inc = 1L
      val res = mutable.Buffer[Long]()
      while (inc <= time) {
        val c = inc * (time - inc)
        if (c > distance) {
          res.append(inc)
        }
        inc += 1
      }
      res.toList
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
    races.map(_.wayToWin.length).product
  }

  def part2(value: String): Long = {
    val races = parse(value)
    val tmp = races.foldLeft(("", "")) { (acc, c) => (acc._1 + c.time.toString, acc._2 + c.distance.toString) }
    val race = Race(tmp._1.toLong, tmp._2.toLong)
    race.wayToWin.length
  }

  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }

}
