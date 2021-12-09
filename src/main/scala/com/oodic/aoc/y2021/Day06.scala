package com.oodic.aoc.y2021

object Day06 extends Puzzle2021[List[Long], Long, Long] {
  override val input: List[Long] =
    getInputFile
      .head
      .split(',')
      .map(_.toLong)
      .toList

  private def numberFishCreated(fish: Long, days: Long, map: Map[(Long, Long), Long]): Map[(Long, Long), Long] =
    if (days > fish) {
      val firstNumberCreated = ((days - fish - 1) / 7L) + 1L
      val (newMap, sum) = (0L until firstNumberCreated).foldLeft((map, 0L))
      {
        case((currentMap, currentSum), index) =>
          val nextDays = (days - fish - (index * 7L)) - 1
          currentMap.get((nextDays, 8)) match {
            case Some(value) => (currentMap, currentSum + value)
            case _ =>
              val newMap = numberFishCreated(8, nextDays, currentMap)
              (newMap, currentSum + newMap.getOrElse((nextDays, 8L), 0L))
          }
      }
      newMap.updated((days, fish), sum + firstNumberCreated)
    }
    else
      map.updated((days, fish), 0L)

  private def execute(fishList: List[Long], days: Long): Long =
    fishList.foldLeft((Map.empty[(Long, Long), Long], 0L)) {
      case ((map, sum), fish) =>
        val newMap = numberFishCreated(fish, days, map)
        (newMap, sum + newMap.getOrElse((days, fish), 0L))
    }._2 + fishList.size

  override def part1(input: List[Long]): Long =
    execute(input, 80)

  override def part2(input: List[Long]): Long =
    execute(input, 256)
}
