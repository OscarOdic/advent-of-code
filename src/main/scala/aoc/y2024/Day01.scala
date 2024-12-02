package aoc.y2024

object Day01 extends Puzzle2024[List[(Int, Int)], Int, Int] {
  override val input: List[(Int, Int)] = getInputFile.map(
    _.split("""\s+""") match
      case Array(left, right) => (left.toInt, right.toInt)
  )

  override def part1(input: List[(Int, Int)]): Int =
    val (leftList, rightList) = input.unzip
    (leftList.sorted.zip(rightList.sorted) map {
      case (left, right) => math.abs(right - left)
    }).sum

  override def part2(input: List[(Int, Int)]): Int =
    val (leftList, rightList) = input.unzip
    leftList.foldLeft(0)((acc, value) =>
      acc + (value * rightList.count(_ == value))
    )
}
