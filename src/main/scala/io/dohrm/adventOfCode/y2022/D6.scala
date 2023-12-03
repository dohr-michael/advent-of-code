package io.dohrm.adventOfCode.y2022

import scala.collection.mutable

object D6 extends Base {

  private def indexOfXFirstDiff(value: String, count: Int): Int = {
    val acc: mutable.IndexedBuffer[Char] = mutable.IndexedBuffer()
    var idx: Int = 0
    while ((acc.size != count || acc.size == count && acc.toSet.size != count) && idx < value.length) {
      if (acc.size == count) {
        acc.remove(0)
      }
      acc.append(value.charAt(idx))
      idx += 1
    }
    idx
  }


  def part1(value: String): Int = indexOfXFirstDiff(value, 4)
  def part2(value: String): Int = indexOfXFirstDiff(value, 14)

  def main(args: Array[String]): Unit = println(part2(read))
}
