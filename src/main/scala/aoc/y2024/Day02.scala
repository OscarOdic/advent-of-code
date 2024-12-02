package aoc.y2024

object Day02 extends Puzzle2024[List[List[Int]], Int, Int] {
  override val input: List[List[Int]] = getInputFile.map(_.split("""\s""").toList.map(_.toInt))

  private def isSafe(list: List[Int]): Boolean =
    val sortedList = list.sorted
    (list == sortedList || list == sortedList.reverse) &&
      list.sliding(2).forall {
        case List(left, right) =>
          val diff = math.abs(right - left)
          diff >= 1 && diff <= 3
      }

  private def isSafeByRemovingValue(list: List[Int]): Boolean =
    list.indices.exists(i =>
      isSafe(list.patch(i, Nil, 1))
    )

  override def part1(input: List[List[Int]]): Int = input.count(isSafe)

  override def part2(input: List[List[Int]]): Int =
    input.count(list =>
      isSafe(list) || isSafeByRemovingValue(list)
    )
}
