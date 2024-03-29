package io.dohrm.adventOfCode.y2023

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.collection.parallel.CollectionConverters.*

object D5 extends Base {
  implicit def ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  case class Range(destination: Long, source: Long, length: Long) {
    def bounded(value: Long): Boolean = value >= source && value <= source + length
    def boundedRevert(value: Long): Boolean = destination <= value && value < destination + length
  }

  case class Parameters(
      seed: Long,
      soil: Long,
      fertilizer: Long,
      water: Long,
      light: Long,
      temperature: Long,
      humidity: Long,
      location: Long
  )
  case class Almanac(
      seeds: List[Long] = Nil,
      parameters: Map[String, List[Range]] = Map.empty
  ) {
    lazy val seedsPairs: List[(Long, Long)] =
      seeds
        .sliding(2, 2)
        .map(v => v.head -> v.last)
        .toList
    def seedToSoil: List[Range] = parameters.getOrElse("seed-to-soil", Nil)
    def soilToFertilizer: List[Range] = parameters.getOrElse("soil-to-fertilizer", Nil)
    def fertilizerToWater: List[Range] = parameters.getOrElse("fertilizer-to-water", Nil)
    def waterToLight: List[Range] = parameters.getOrElse("water-to-light", Nil)
    def lightToTemperature: List[Range] = parameters.getOrElse("light-to-temperature", Nil)
    def temperatureToHumidity: List[Range] = parameters.getOrElse("temperature-to-humidity", Nil)
    def humidityToLocation: List[Range] = parameters.getOrElse("humidity-to-location", Nil)

    private def items(toFind: Long, reference: List[Range]): Long = {
      val base = reference.find(_.bounded(toFind))
      base.map(v => v.destination + (toFind - v.source)).getOrElse(toFind)
    }

    private def itemsRevert(toFind: Long, reference: List[Range]): Long = {
      val base = reference.find(_.boundedRevert(toFind))
      base.map(v => v.source + (toFind - v.destination)).getOrElse(toFind)
    }

    private def getParameters(seed: Long): Parameters = {
      val soil = items(seed, seedToSoil)
      val fertilizer = items(soil, soilToFertilizer)
      val water = items(fertilizer, fertilizerToWater)
      val light = items(water, waterToLight)
      val temperature = items(light, lightToTemperature)
      val humidity = items(temperature, temperatureToHumidity)
      val location = items(humidity, humidityToLocation)
      Parameters(seed, soil, fertilizer, water, light, temperature, humidity, location)
    }

    private def getParametersRevert(location: Long): Parameters = {
      val humidity = itemsRevert(location, humidityToLocation)
      val temperature = itemsRevert(humidity, temperatureToHumidity)
      val light = itemsRevert(temperature, lightToTemperature)
      val water = itemsRevert(light, waterToLight)
      val fertilizer = itemsRevert(water, fertilizerToWater)
      val soil = itemsRevert(fertilizer, soilToFertilizer)
      val seed = itemsRevert(soil, seedToSoil)
      Parameters(seed, soil, fertilizer, water, light, temperature, humidity, location)
    }

    lazy val lowestParameter: Option[Parameters] =
      seeds.foldLeft[Option[Parameters]](None) { (acc, seed) =>
        val p = getParameters(seed)
        acc.filter(_.location < p.location).orElse(Some(p))
      }

    lazy val lowestParameterFromParis: Option[Parameters] = {
      val location = new AtomicLong(0L)
      var loc = 0
      var current: Option[Parameters] = None
      var finished = false
      while (!finished) {
        val p = getParametersRevert(loc)
        if (seedsPairs.exists(bound => bound._1 <= p.seed && p.seed <= (bound._1 + bound._2))) {
          current = Some(p)
          finished = true
        }
        loc += 1
      }
      current
    }
  }

  def parse(value: String): Almanac = {
    val items = value.split("\n")
    var counter = 0
    var res = Almanac()
    while (counter < items.length) {
      val current = items(counter).trim
      if (current.startsWith("seeds:")) {
        res = res.copy(seeds = current.stripPrefix("seeds:").split(' ').filter(_.nonEmpty).map(_.trim.toLong).toList)
      } else if (current.endsWith("map:")) {
        counter += 1
        var next = items(counter).trim
        val ranges = mutable.Buffer[Range]()
        while (next != "") {
          val parts = next.split(' ').filter(_.nonEmpty).map(_.trim.toLong)
          ranges.addOne(Range(parts(0), parts(1), parts(2)))
          counter += 1
          if (counter < items.length) {
            next = items(counter).trim
          } else {
            next = ""
          }
        }
        res = res.copy(
          parameters = res.parameters + (current.stripSuffix("map:").trim -> ranges.toList)
        )
      }
      counter += 1
    }
    res
  }

  def part1(value: String): Long = {
    val almanac = parse(value)
    val items = almanac.lowestParameter
    items.map(_.location).getOrElse(-1)
  }
  def part2(value: String): Long = {
    val almanac = parse(value)
    val items = almanac.lowestParameterFromParis
    items.map(_.location).getOrElse(-1)
  }
  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read)) // 4917124
  }

}
