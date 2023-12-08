package io.dohrm.adventOfCode.y2023
import scala.annotation.tailrec
import scala.collection.{SortedMap, mutable}

object D8 extends Base {
  sealed trait Move {
    def next(item: Item): String
  }
  object Move {
    case object Left extends Move {
      override def next(item: Item): String = item.left
    }
    case object Right extends Move {
      override def next(item: Item): String = item.right
    }
  }
  case class Item(name: String, left: String, right: String)

  case class Tree(nodes: List[Item]) {
    lazy val nodesByName: Map[String, Item] = nodes.map(n => n.name -> n).toMap

    private def loop[C](start: C, next: (C, Move) => C, end: C => Boolean, moves: List[Move]): Long = {
      var count = 0
      var current = start
      var move = 0
      while (!end(current)) {
        current = next(current, moves(move))
        move = if (move == moves.length - 1) 0 else move + 1
        count += 1
      }
      count
    }

    def countE1(moves: List[Move]): Long = {
      loop[Item](
        nodesByName("AAA"),
        (c, move) => nodesByName(move.next(c)),
        _.name == "ZZZ",
        moves
      )
    }

    def countE2v1(moves: List[Move]): Long = {
      loop[List[Item]](
        nodes.filter(_.name.endsWith("A")),
        (c, move) => c.map(r => nodesByName(move.next(r))),
        _.forall(_.name.endsWith("Z")),
        moves
      )
    }

    def countE2v2(moves: List[Move]): Long = {
      val items = nodes.filter(_.name.endsWith("A"))
      val positions = items.map(v =>
        loop[Item](
          v,
          (c, move) => nodesByName(move.next(c)),
          _.name.endsWith("Z"),
          moves
        )
      )

      // LCM
      val ranges = positions.toArray
      (0 until (ranges.length - 1)).foreach { idx =>
        val greater = Math.max(ranges(idx), ranges(idx + 1))
        var currentMul = greater
        while (currentMul % ranges(idx) != 0 || currentMul % ranges(idx + 1) != 0) {
          currentMul += greater
        }
        ranges.update(idx + 1, currentMul)
      }
      ranges.last
    }

  }
  def parse(input: String): (List[Move], Tree) = {
    val items = input.split("\n")
    val moves = items.head.toCharArray.collect {
      case 'R' => Move.Right
      case 'L' => Move.Left
    }
    val nodes = items.tail
      .filter(_.nonEmpty)
      .map { item =>
        val base = item.split('=')
        val name = base.head.trim
        val lr = base.last.trim.stripPrefix("(").stripSuffix(")").split(",").map(_.trim)
        Item(name, lr.head, lr.last)
      }
    moves.toList -> Tree(nodes.toList)
  }

  def part1(value: String): Long = {
    val (moves, tree) = parse(value)
    tree.countE1(moves)
  }
  def part2(value: String): Long = {
    val (moves, tree) = parse(value)
    tree.countE2v2(moves)
  }
  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }

}
