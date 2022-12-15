package io.dohrm.adventOfCode2022
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.util.Try

object D5 extends Base {
  case class Crates(stacks: Map[String, List[String]]) {
    private def doMove(move: Move, reverse: Boolean): Crates = (for {
      from <- stacks.get(move.from.toString).map(_.reverse)
      to <- stacks.get(move.to.toString)
      toPush = from.take(move.count)
    } yield Crates(
      stacks ++ List(
        move.from.toString -> from.reverse.slice(0, from.length - toPush.length),
        move.to.toString -> (to ++ (if (reverse) toPush.reverse else toPush))
      )
    )).getOrElse(this)
    def move(move: Move): Crates = doMove(move, reverse = false)

    def moveAndKeepOrder(move: Move): Crates = doMove(move, reverse = true)

    def head: String = stacks
      .map(c => c._1.toInt -> c._2.lastOption.getOrElse(" "))
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString
  }

  object Crates {
    def apply(value: Array[String], nbStacks: Int): Crates = {
      val acc = mutable.Map[String, mutable.Queue[String]]()
      value.foreach { row =>
        var rest = row
        for {
          i <- 1 to nbStacks
        } yield {
          val current = if (rest.length >= 3) rest.substring(0, 3).trim else ""
          rest = if (rest.length >= 3) rest.substring(3).stripPrefix(" ") else ""
          val queue = acc.getOrElseUpdate(i.toString, mutable.Queue())
          if (current != "") queue.append(current.stripPrefix("[").stripSuffix("]"))
          ()
        }
      }
      Crates(acc.map(c => c._1 -> c._2.toList).toMap)
    }
  }
  case class Move(count: Int, from: Int, to: Int)

  object Move {
    private val MoveRegex = "move (\\d+) from (\\d+) to (\\d+)".r
    def apply(value: String): Option[Move] = {
      value.trim match {
        case MoveRegex(as, bs, cs) =>
          for {
            a <- Try(as.toInt).toOption
            b <- Try(bs.toInt).toOption
            c <- Try(cs.toInt).toOption
          } yield Move(a, b, c)
        case _ => None
      }
    }
  }

  def parse(value: String): (Crates, List[Move]) = {
    val items = value.split('\n')
    val separatorIndex = items.indexWhere(_.trim == "")
    if (separatorIndex < 0) return (Crates(Map.empty), List.empty)
    val rawCrates = items.slice(0, separatorIndex)
    val rawMoves = items.slice(separatorIndex + 1, items.length)
    val nbStacks = rawCrates.last
      .split(' ')
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(c => Try(c.toInt).toOption.toList)
      .max
    val crates = Crates(rawCrates.reverse.tail, nbStacks)
    val moves = rawMoves.toList.flatMap(v => Move(v).toList)
    (crates, moves)
  }

  def part1(value: String): String = {
    val (crates, moves) = parse(value)
    val result = moves.foldLeft(crates) { (acc, c) => acc.move(c) }
    result.head
  }

  def part2(value: String): String = {
    val (crates, moves) = parse(value)
    val result = moves.foldLeft(crates) { (acc, c) => acc.moveAndKeepOrder(c) }
    result.head
  }

  def main(args: Array[String]): Unit = {
    println(part2(read))
  }

}
