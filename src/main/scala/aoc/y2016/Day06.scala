package aoc.y2016

object Day06 extends Puzzle2016[List[String], String, String] {
  override val input: List[String] = getInputFile
    .map(_.toList).transpose.map(_.mkString)

  private type Order = Boolean
  private val max = false
  private val min = true

  private def resolve(input: List[String], order: Order = max) = {
    val function: List[(Char, Int)] => (Char, Int) = group =>
      if (order == max) group.maxBy(_._2)
      else group.minBy(_._2)

    input.map( column =>
      function(column.groupBy(identity).view.mapValues(_.length).toMap.toList)._1
    ).mkString
  }

  override def part1(input: List[String]): String = resolve(input)

  override def part2(input: List[String]): String = resolve(input, min)
}
