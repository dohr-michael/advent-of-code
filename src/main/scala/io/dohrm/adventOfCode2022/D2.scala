package io.dohrm.adventOfCode2022

object D2 extends Base {
  override def sample: String =
    """A Y
    |B X
    |C Z""".stripMargin

  enum Result(val player: String, val value: Int):
    case Lose extends Result("X", 0)
    case Draw extends Result("Y", 3)
    case Win extends Result("Z", 6)

  enum Choice(val opponent: String, val player: String, val value: Int):
    case Rock extends Choice("A", "X", 1)
    case Paper extends Choice("B", "Y", 2)
    case Scissors extends Choice("C", "Z", 3)

  private def result(opponent: Choice, player: Choice): Result = {
    (opponent, player) match
      case (a, b) if a == b => Result.Draw
      case (Choice.Rock, Choice.Paper) | (Choice.Paper, Choice.Scissors) | (Choice.Scissors, Choice.Rock) =>
        Result.Win
      case _ => Result.Lose
  }

  private def result(opponent: Choice, expected: Result): Choice =
    expected match
      case Result.Draw => opponent
      case Result.Lose =>
        opponent match
          case Choice.Rock     => Choice.Scissors
          case Choice.Paper    => Choice.Rock
          case Choice.Scissors => Choice.Paper
      case Result.Win =>
        opponent match
          case Choice.Rock     => Choice.Paper
          case Choice.Paper    => Choice.Scissors
          case Choice.Scissors => Choice.Rock

  private def part1(value: String): Int = {
    val s = value.split(' ').map(_.trim)
    if (s.length != 2) return 0
    (Choice.values.find(_.opponent == s(0)), Choice.values.find(_.player == s(1))) match
      case (Some(a), Some(b)) => result(a, b).value + b.value
      case _                  => 0
  }

  private def part2(value: String): Int = {
    val s = value.split(' ').map(_.trim)
    if (s.length != 2) return 0
    (Choice.values.find(_.opponent == s(0)), Result.values.find(_.player == s(1))) match
      case (Some(a), Some(b)) => result(a, b).value + b.value
      case _                  => 0
  }

  private def readAll(fn: String => Int, value: String): Int =
    value.split("\n").foldLeft(0)(_ + fn(_))

  // 15572

  def main(args: Array[String]): Unit =
    readAll(part2, read)

}
