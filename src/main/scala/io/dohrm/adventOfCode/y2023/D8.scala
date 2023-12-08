package io.dohrm.adventOfCode.y2023
import scala.annotation.tailrec
import scala.collection.{SortedMap, mutable}

object D8 extends Base {
  sealed trait Move
  object Move {
    case object Left extends Move
    case object Right extends Move
  }
  case class Node(name: String, left: String, right: String)
  sealed trait BiTree {
    def name: String
    def exists(name: String): Boolean
  }
  case class Branch(name: String, left: Option[BiTree], right: Option[BiTree]) extends BiTree {
    override def exists(name: String): Boolean = left.exists(_.exists(name)) || right.exists(_.exists(name))
  }
  case class Leaf(name: String) extends BiTree {
    override def exists(name: String): Boolean = name == this.name
  }

  case class Tree(nodes: List[Node]) {
    lazy val nodesByName: Map[String, Node] = nodes.map(n => n.name -> n).toMap
    def asBiTree(first: String, last: String): BiTree = {
      val cache = mutable.HashMap[String, Option[BiTree]]()
      def build(name: String): Option[BiTree] = {
        cache.getOrElse(
          name, {
            if (name == last) {
              Some(Leaf(name))
            } else if (name == first) {
              None
            } else {
              val n = nodesByName(name)
              (build(n.left), build(n.right)) match {
                case (None, None) => Some(Leaf(name))
                case (a, b)       => Some(Branch(name, a, b))
              }
            }
          }
        )
      }
      val b = nodesByName(first)
      Branch(first, build(b.left), build(b.right))
    }
    def applyMoves(starting: String, moves: List[Move]): Node = {
      var res = nodesByName(starting)
      moves.foreach {
        case Move.Left =>
          res = nodesByName(res.left)
        case Move.Right =>
          res = nodesByName(res.right)
      }
      res
    }

    def countUntil(from: String, to: String): Int = {
      val biTree = asBiTree(from, to)
      println(biTree)
      println(biTree.exists(to))
      0
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
        Node(name, lr.head, lr.last)
      }
    moves.toList -> Tree(nodes.toList)
  }

  def part1(value: String): Int = {
    val (moves, tree) = parse(value)
    val afterMove = tree.applyMoves("AAA", moves)
    println(tree.asBiTree(afterMove.name, "ZZZ"))
    tree.countUntil(afterMove.name, "ZZZ")
  }
  def part2(value: String): Int = 0
  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }

}
