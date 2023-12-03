package io.dohrm.adventOfCode.y2022

import scala.util.Try

object D4 extends Base {
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

  def parse(value: String): List[Elves] = value
    .split('\n')
    .map(_.trim)
    .flatMap(Elves(_).toList)
    .toList

  def part1(value: String): Int = parse(value).count(_.fullyIncluded)
  def part2(value: String): Int = parse(value).count(_.overlapped)

  def main(args: Array[String]): Unit = {
    println(part2(read))
  }

}
