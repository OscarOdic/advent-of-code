package aoc.y2020

object Day06 extends Puzzle2020[List[List[String]], Int, Int] {
  override val input: List[List[String]] =
    getInputFile.foldLeft(List(List.empty[String])) ((list, line) =>
      if (line != "") list match {
        case h :: t => (h :+ line) :: t
      }
      else
        List.empty[String] +: list
    ).reverse

  override def part1(input: List[List[String]]): Int =
    input
      .map(_.flatten.distinct.size)
      .sum

  override def part2(input: List[List[String]]): Int =
    input
      .map(_.reduce(_ intersect _).length)
      .sum
}
