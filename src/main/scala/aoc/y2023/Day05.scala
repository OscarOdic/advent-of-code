package aoc.y2023

import scala.util.parsing.combinator.RegexParsers

object Day05 extends Puzzle2023[List[String], Long, Long] with RegexParsers {
  override val input: List[String] = getInputFile

  private case class Conversion(destRangeStart: Long, srcRangeStart: Long, rangeLength: Long) {
    private val srcRangeEnd: Long = srcRangeStart + rangeLength - 1
    private val diff: Long = destRangeStart - srcRangeStart

    def isRange(seed: Long): Boolean =
      (seed >= srcRangeStart) && (seed <= srcRangeStart + rangeLength)

    def convert(seed: Long): Long =
      destRangeStart + seed - srcRangeStart

    def convertInterval(intervalSeeds: (Long, Long)): (Option[(Long, Long)], List[(Long, Long)]) = {
      val startIn = math.max(srcRangeStart, intervalSeeds._1)
      val endIn = math.min(srcRangeEnd, intervalSeeds._2)

      if (startIn > endIn) (None, List(intervalSeeds))
      else (
        Some((startIn + diff, endIn + diff)),
        List(
          if (intervalSeeds._1 < startIn) Some((intervalSeeds._1, startIn - 1)) else None,
          if (endIn < intervalSeeds._2) Some((endIn + 1, intervalSeeds._2)) else None
        ).flatten
      )
    }
  }

  private case class Conversions(conversions: List[Conversion]) {
    def convert(seed: Long): Long = {
      conversions.find(_.isRange(seed)) match
        case Some(conversion) => conversion.convert(seed)
        case _ => seed
    }

    def convertIntervals(intervalsSeeds: List[(Long, Long)]): List[(Long, Long)] =
      val (notConverted, converted) = conversions.foldLeft((intervalsSeeds, List.empty[(Long, Long)])) {
        case ((seedsToConvert, convertedSeeds), conversion) =>
          val result = seedsToConvert.map(conversion.convertInterval)
          (result.flatMap(_._2), convertedSeeds ++ result.flatMap(_._1))
      }

      notConverted ++ converted
  }

  private case class Almanac(seeds: List[Long], conversionsList: List[Conversions]) {
    private def getLocation(seed: Long): Long = {
      conversionsList.foldLeft(seed)((currentSeed, conversions) =>
        conversions.convert(currentSeed)
      )
    }

    private def getIntervalsLocation(intervalSeeds: (Long, Long)): List[(Long, Long)] =
      conversionsList.foldLeft(List(intervalSeeds))((currentIntervals, conversions) =>
        conversions.convertIntervals(currentIntervals)
      )

    def getMinLocation: Long =
      seeds
        .map(getLocation)
        .min

    def getMinIntervalsLocation: Long =
      seeds
        .grouped(2)
        .flatMap { case List(start, range) => getIntervalsLocation((start, start + range - 1)) }
        .minBy(_._1)._1
  }

  private def seedsParser: Parser[List[Long]] = "seeds:" ~> rep1("""\d+""".r ^^ (_.toLong))

  private def conversionParser: Parser[Conversion] =
    for {
      destRangeStart <- """\d+""".r ^^ (_.toLong)
      srcRangeStart <- """\d+""".r ^^ (_.toLong)
      rangeLength <- """\d+""".r ^^ (_.toLong)
    } yield Conversion(destRangeStart, srcRangeStart, rangeLength)

  private def parseConversionsList(input: List[String]): List[Conversions] =
    if (input.isEmpty) Nil
    else {
      val conversionList = input.takeWhile(_ != "")
      val conversions = Conversions(
        conversionList.tail.map(parse(conversionParser, _).get)
      )
      conversions +: parseConversionsList(input.dropWhile(_ != "").drop(1))
    }

  private def parseAlmanac(input: List[String]): Almanac =
    Almanac(
      parse(seedsParser, input.head).get,
      parseConversionsList(input.drop(2))
    )

  override def part1(input: List[String]): Long =
    parseAlmanac(input)
      .getMinLocation

  override def part2(input: List[String]): Long =
    parseAlmanac(input)
      .getMinIntervalsLocation
}
