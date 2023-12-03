package io.dohrm.adventOfCode.y2023

object D2 extends Base {
  final case class Cubes(green: Int = 0, blue: Int = 0, red: Int = 0)
  final case class Game(id: Int, subsets: List[Cubes]) {
    lazy val maxBlue: Int = subsets.map(_.blue).max
    lazy val maxRed: Int = subsets.map(_.red).max
    lazy val maxGreen: Int = subsets.map(_.green).max
    def possible(red: Int, green: Int, blue: Int): Boolean =
      blue >= maxBlue && red >= maxRed && green >= maxGreen

    def power: Int = maxRed * maxGreen * maxBlue
  }

  def parseGame(row: String): Game = {
    val base = row.stripPrefix("Game ").split(':')
    val id = base.head.toInt
    Game(
      base.head.toInt,
      base.last.trim
        .split(';')
        .map { current =>
          current.trim.split(',').map(_.trim).foldLeft(Cubes()) { (acc, c) =>
            if (c.endsWith("blue")) acc.copy(blue = c.stripSuffix(" blue").toInt)
            else if (c.endsWith("red")) acc.copy(red = c.stripSuffix(" red").toInt)
            else if (c.endsWith("green")) acc.copy(green = c.stripSuffix(" green").toInt)
            else acc
          }
        }
        .toList
    )
  }

  def read(value: String): List[Game] =
    value
      .split("\n")
      .map(parseGame)
      .toList
  def part1(value: String): Int = {
    val games = read(value)
    games.filter(_.possible(12, 13, 14)).map(_.id).sum
  }

  def part2(value: String): Int = {
    val games = read(value)
    games.map(_.power).sum
  }

  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }
}
