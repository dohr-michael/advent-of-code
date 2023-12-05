package io.dohrm.adventOfCode.y2023

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object D5 extends Base {
  implicit def ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  case class Range(destination: Long, source: Long, length: Long) {
    def sourceToDestination(value: Long): Option[Long] =
      if (value >= source && value <= source + length) {
        Some(destination + value - source)
      } else {
        None
      }
  }

  case class Parameters(
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
      seeds.sliding(2).map(v => v.head -> v.last).toList
    def seedToSoil: List[Range] = parameters.getOrElse("seed-to-soil", Nil)
    def soilToFertilizer: List[Range] = parameters.getOrElse("soil-to-fertilizer", Nil)
    def fertilizerToWater: List[Range] = parameters.getOrElse("fertilizer-to-water", Nil)
    def waterToLight: List[Range] = parameters.getOrElse("water-to-light", Nil)
    def lightToTemperature: List[Range] = parameters.getOrElse("light-to-temperature", Nil)
    def temperatureToHumidity: List[Range] = parameters.getOrElse("temperature-to-humidity", Nil)
    def humidityToLocation: List[Range] = parameters.getOrElse("humidity-to-location", Nil)

    private def items(toFind: Long, reference: List[Range]): Long = {
      val base = reference
        .map(_.sourceToDestination(toFind))
        .collect { case Some(x) => x }
      if (base.isEmpty) {
        toFind
      } else {
        base.head
      }
    }

    lazy val resultsFromSeeds: Map[Long, Parameters] = {
      (for {
        seed <- seeds
        soil = items(seed, seedToSoil)
        fertilizer = items(soil, soilToFertilizer)
        water = items(fertilizer, fertilizerToWater)
        light = items(water, waterToLight)
        temperature = items(light, lightToTemperature)
        humidity = items(temperature, temperatureToHumidity)
        location = items(humidity, humidityToLocation)
      } yield seed -> Parameters(soil, fertilizer, water, light, temperature, humidity, location)).toMap
    }

    lazy val resultsFromPairs: Future[List[Parameters]] = {
      Future
        .traverse(seedsPairs) { seedP =>
          Future {
            var param: Option[Parameters] = None
            var seed = seedP._1
            while (seed <= seedP._2 + seedP._1) {
              val soil = items(seed, seedToSoil)
              val fertilizer = items(soil, soilToFertilizer)
              val water = items(fertilizer, fertilizerToWater)
              val light = items(water, waterToLight)
              val temperature = items(light, lightToTemperature)
              val humidity = items(temperature, temperatureToHumidity)
              val location = items(humidity, humidityToLocation)
              val p = Parameters(
                soil,
                fertilizer,
                water,
                light,
                temperature,
                humidity,
                location
              )
              param = param.filter(_.location < p.location).orElse(Some(p))
              seed += 1
            }
            param
          }
        }
        .map(_.collect { case Some(p) => p })
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
    val items = almanac.resultsFromSeeds
    items
      .map(_._2.location)
      .min
  }
  def part2(value: String): Long = {
    val almanac = parse(value)
    Await.result(
      almanac.resultsFromPairs
        .map(_.map(_.location).min),
      Duration.Inf
    )
  }
  def main(args: Array[String]): Unit = {
    println(part1(read))
    println(part2(read))
  }

}
