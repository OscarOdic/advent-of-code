package aoc.y2023

object Day11 extends Puzzle2023[List[String], Long, Long] {
  override val input: List[String] = getInputFile

  private def getEmptyLines(universe: List[String]): List[Long] =
    for {
      (line, y) <- universe.zipWithIndex
      if line.forall(_ == '.')
    } yield y.toLong

  private def getDistanceAxe(value1: Long, value2: Long, emptyLines: List[Long], size: Long): Long =
    (math.min(value1, value2) until math.max(value1, value2)).map(value =>
      if (emptyLines.contains(value)) size else 1L
    ).sum

  private def sumShortestPaths(universe: List[String], size: Long = 2): Long = {
    val emptyY = getEmptyLines(universe)
    val emptyX = getEmptyLines(universe.transpose.map(_.mkString))

    (for {
      (line, y) <- universe.zipWithIndex
      (char, x) <- line.zipWithIndex
      if char == '#'
    } yield (x.toLong, y.toLong))
      .combinations(2).map { case List((x1, y1), (x2, y2)) =>
        getDistanceAxe(x1, x2, emptyX, size) + getDistanceAxe(y1, y2, emptyY, size)
      }
      .sum
  }

  override def part1(input: List[String]): Long =
    sumShortestPaths(input)

  override def part2(input: List[String]): Long =
    sumShortestPaths(input, 1000000)
}
