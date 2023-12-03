package io.dohrm.adventOfCode.y2023
import scala.collection.mutable
object D3 extends Base {
  case class Number(row: Int, scol: Int, ecol: Int, value: Int) {
    def isBounded(row: Int, col: Int): Boolean =
      this.row == row &&
        scol <= col &&
        col <= ecol
  }
  case class Symbol(row: Int, col: Int, value: Char)
  case class Engine(
      numbers: List[Number],
      symbols: List[Symbol]
  ) {
    def findAdjacentNumbers: List[(Symbol, List[Number])] =
      symbols.map { symbol =>
        symbol -> numbers.filter { v =>
          v.isBounded(symbol.row, symbol.col - 1) ||
          v.isBounded(symbol.row, symbol.col + 1) ||
          v.isBounded(symbol.row - 1, symbol.col) ||
          v.isBounded(symbol.row + 1, symbol.col) ||
          v.isBounded(symbol.row - 1, symbol.col - 1) ||
          v.isBounded(symbol.row - 1, symbol.col + 1) ||
          v.isBounded(symbol.row + 1, symbol.col - 1) ||
          v.isBounded(symbol.row + 1, symbol.col + 1)
        }
      }
  }

  def isNumber(v: Char): Boolean = v >= '0' && v <= '9'
  def isSymbol(v: Char): Boolean = v != '.' && v.toString != "" && !isNumber(v)
  def parse(value: String): Engine = {
    val split = value.split("\n")
    val numbers: List[Number] = split.zipWithIndex.toList.flatMap { case (row, idx) =>
      val result = mutable.Buffer[Number]()
      var x = 0
      while (x < row.length) {
        if (isNumber(row(x))) {
          val scol = x
          val sb = new StringBuilder()
          while (x < row.length && isNumber(row(x))) {
            sb.append(row(x))
            x += 1
          }
          result.append(Number(idx, scol, x - 1, sb.toString().toInt))
        } else {
          x += 1
        }
      }
      result.toList
    }
    Engine(
      numbers,
      split.zipWithIndex.toList.flatMap { case (row, x) =>
        row.trim.zipWithIndex.toList.flatMap { case (col, y) =>
          if (isSymbol(col)) List(Symbol(x, y, col))
          else Nil
        }
      }
    )
  }
  def part1(value: String): Int = {
    val engine = parse(value)
    engine.findAdjacentNumbers.flatMap(_._2).map(_.value).sum
  }
  def part2(value: String): Int = {
    val engine = parse(value)
    engine.findAdjacentNumbers
      .filter { case (s, value) =>
        s.value == '*' && value.length == 2
      }
      .map(_._2.map(_.value).product)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }
}
