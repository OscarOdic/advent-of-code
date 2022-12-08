package aoc.y2022

object Day03 extends Puzzle2022[List[String], Int, Int] {
  override val input: List[String] = getInputFile

  private def charToInt(c: Char): Int =
    if ("[A-Z]".r.matches(c.toString))
      c.toInt - 38
    else
      c.toInt - 96

  override def part1(input: List[String]): Int =
    input
      .map(value => (
        value.substring(0, value.length/2),
        value.substring(value.length/2, value.length)
      ))
      .map {
        case (left, right) => left.intersect(right).head
      }
      .map(charToInt)
      .sum

  override def part2(input: List[String]): Int =
    input
      .grouped(3)
      .map(_.reduce(_ intersect _).head)
      .map(charToInt)
      .sum
}
