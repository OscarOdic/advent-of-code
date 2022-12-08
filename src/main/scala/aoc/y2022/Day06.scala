package aoc.y2022

object Day06 extends Puzzle2022[String, Int, Int] {
  override val input: String = getInputFile.head

  private def allDifferent(value: String) =
    value.forall(v => value.count(_ == v) == 1)

  private def resolve(input: String, distinctCharacters: Int): Int =
    input
      .sliding(distinctCharacters)
      .zipWithIndex
      .map { case (value, index) => (value, index + distinctCharacters) }
      .find { case (value, _) => allDifferent(value) }
      .get
      ._2

  override def part1(input: String): Int =
    resolve(input, 4)

  override def part2(input: String): Int =
    resolve(input, 14)
}
