package aoc.y2021

object Day07 extends Puzzle2021[List[Int], Int, Int] {
  override val input: List[Int] = {
    getInputFile
      .head
      .split(',').toList
      .map(_.toInt)
  }

  private def sumWholeNumbers(n: Int): Int =
    n * (n + 1) / 2

  private def align(crabs: List[Int], index: Int)(fuelFunction: (Int, Int, Int) => Int): Int =
    crabs.foldLeft(0)((fuel, crab) => fuelFunction(fuel, crab, index))

  private def minAlignment(crabs: List[Int])(fuelFunction: (Int, Int, Int) => Int): Int =
    (crabs.min to crabs.max)
      .map(index => align(crabs, index)(fuelFunction))
      .min

  override def part1(input: List[Int]): Int =
    minAlignment(input)((fuel, crab, index) =>
      fuel + math.abs(crab - index)
    )

  override def part2(input: List[Int]): Int =
    minAlignment(input)((fuel, crab, index) =>
      fuel + sumWholeNumbers(math.abs(crab - index))
    )
}
