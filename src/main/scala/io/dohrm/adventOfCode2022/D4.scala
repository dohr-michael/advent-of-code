package io.dohrm.adventOfCode2022
import scala.util.Try

object D4 extends Base {

  override def sample: String =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin
  case class Elf(from: Int, to: Int) {
    private def fullyIncluded(a: Elf, b: Elf): Boolean =
      b.from >= a.from && b.from <= a.to && b.to >= a.from && b.to <= a.to
    private def overlapped(a: Elf, b: Elf): Boolean =
      b.from <= a.to && b.from >= a.from || b.to <= a.to && b.to >= a.from

    def fullyIncluded(other: Elf): Boolean = fullyIncluded(this, other) || fullyIncluded(other, this)

    def overlapped(other: Elf): Boolean = overlapped(this, other) || overlapped(other, this)
  }
  object Elf {
    def apply(value: String): Option[Elf] = {
      val tmp = value.split('-').map(c => Try(c.trim.toInt).toOption).collect { case Some(x) => x }
      if (tmp.length == 2) Some(Elf(tmp.min, tmp.max))
      else None
    }
  }

  case class Elves(left: Elf, right: Elf) {
    def fullyIncluded: Boolean = left.fullyIncluded(right)
    def overlapped: Boolean = left.overlapped(right)
  }

  object Elves {
    def apply(value: String): Option[Elves] = {
      val tmp = value.split(',').map(_.trim)
      if (tmp.length == 2)
        for {
          left <- Elf(tmp(0))
          right <- Elf(tmp(1))
        } yield Elves(left, right)
      else None
    }
  }

  def part1(elves: List[Elves]): Unit = println(elves.count(_.fullyIncluded))
  def part2(elves: List[Elves]): Unit = println(elves.count(_.overlapped))
  def process(fn: List[Elves] => Unit, value: String): Unit =
    fn(
      value
        .split('\n')
        .map(_.trim)
        .flatMap(Elves(_).toList)
        .toList
    )

  def main(args: Array[String]): Unit = {
    process(
      part2,
      read
    )
  }

}
