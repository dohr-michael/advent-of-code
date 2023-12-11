package io.dohrm.adventOfCode.y2023
import scala.collection.mutable
object D9 extends Base {
  case class History(items: List[Long]) {
    lazy val sequences: List[List[Long]] = {
      var previous: List[Long] = items
      val res = mutable.Buffer[List[Long]](items)
      while (!previous.forall(_ == 0L)) {
        previous = previous.tail
          .foldLeft((List.empty[Long], previous.head)) { (acc, c) =>
            (acc._1 :+ (c - acc._2)) -> c
          }
          ._1
        res.append(previous)
      }
      res.toList
    }

    lazy val next: Long = {
      val tmp = sequences.reverse
      tmp.tail.foldLeft(0L) { (acc, c) => c.last + acc }
    }

    lazy val prev: Long = {
      val tmp = sequences.reverse
      tmp.tail.foldLeft(0L) { (acc, c) => c.head - acc}
    }

  }

  def parse(value: String): List[History] = {
    value
      .split("\n")
      .map(v => v.split(' ').filter(_.trim.nonEmpty).map(_.trim.toLong))
      .map(h => History(h.toList))
      .toList
  }

  def part1(value: String): Long = {
    val items = parse(value)
    items.map(_.next).sum
  }
  def part2(value: String): Long = {
    val items = parse(value)
    items.map(_.prev).sum
  }
  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }

}
