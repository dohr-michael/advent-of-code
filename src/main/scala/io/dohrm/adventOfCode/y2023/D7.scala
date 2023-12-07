package io.dohrm.adventOfCode.y2023
import io.dohrm.adventOfCode.y2023.D7.Hand.handValues

import scala.collection.mutable
object D7 extends Base { self =>
  val cardsValues = {
    val res = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
    res.reverse.zip((65 until (65 + res.length)).map(_.toChar)).toMap
  }

  val cardsWithJokerValues = {
    val res = List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')
    res.reverse.zip((65 until (65 + res.length)).map(_.toChar)).toMap
  }

  def cardCounts(value: String, useJoker: Boolean): Map[Char, Int] = {
    val res = mutable.HashMap[Char, Int]()
    value.foreach(c => res.update(c, res.getOrElse(c, 0) + 1))
    if (useJoker) {
      res.remove('J') match {
        case Some(c) =>
          res.toList.sortWith { (left, right) =>
            if (left._2 == right._2) {
              val lv = cardsWithJokerValues(left._1)
              val rv = cardsWithJokerValues(right._1)
              lv.compare(rv) < 0
            } else {
              left._2.compare(right._2) < 0
            }
          }.lastOption match {
            case Some(toUpdate) =>
              res.update(toUpdate._1, toUpdate._2 + c)
            case None =>
              res.update('J', c)
          }

        case _ => ()
      }
    }
    res.toMap
  }
  case class Hand(cards: Map[Char, Int], original: String, useJoker: Boolean) extends Ordered[Hand] {
    def size: Int = cards.size
    def exists(fn: ((Char, Int)) => Boolean): Boolean = cards.exists(fn)

    lazy val value: Int = handValues.find(_._1(this)).map(_._2).getOrElse(0)
    lazy val cardValues: String = original.toCharArray
      .foldLeft("") { (acc, c) =>
        acc + (if (useJoker) self.cardsWithJokerValues(c) else self.cardsValues(c)).toString
      }
    override def compare(that: Hand): Int =
      if (value == that.value) cardValues.compare(that.cardValues)
      else value.compare(that.value)
  }
  object Hand {
    def apply(original: String, useJoker: Boolean): Hand = {
      val chars = cardCounts(original, useJoker)
      Hand(chars, original, useJoker)
    }

    lazy val handValues = List(
      (hand: Hand) => hand.size == 1, // 5 of kind
      (hand: Hand) => hand.size == 2 && hand.exists(_._2 == 4), // 4 of kind
      (hand: Hand) => hand.size == 2 && hand.exists(_._2 == 3), // Full house
      (hand: Hand) => hand.size == 3 && hand.exists(_._2 == 3), // 3 of Kind
      (hand: Hand) => hand.size == 3 && hand.exists(_._2 == 1) && hand.exists(_._2 == 2), // 2 pair
      (hand: Hand) => hand.size == 4 && hand.exists(_._2 == 2), // pair
      (hand: Hand) => hand.size == 5
    ).reverse.zipWithIndex.reverse
  }

  def parse(value: String, withJoker: Boolean = false): List[(Hand, Long)] =
    value
      .split("\n")
      .map { v =>
        val tmp = v.split(' ').filter(_.nonEmpty).map(_.trim)
        Hand(tmp.head, withJoker) -> tmp.last.toLong
      }
      .toList
  def part1(value: String): Long = {
    parse(value).sorted.zipWithIndex
      .foldLeft(0L)((acc, c) => acc + (c._2 + 1) * c._1._2)
  }
  def part2(value: String): Long = {
    parse(value, withJoker = true).sorted.zipWithIndex
      .foldLeft(0L)((acc, c) => acc + (c._2 + 1) * c._1._2)
  }
  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read)) // 250401978
  }

}
