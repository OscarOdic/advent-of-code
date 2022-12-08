package aoc.y2016

object Day11 extends Puzzle2016[List[Int], Int, Int] {
  override val input: List[Int] = List(4, 2, 4, 0)

  def resolve(items: List[Int]): Int =
    if (items.last == items.sum) 0
    else {
      val f = items.zipWithIndex.find(i => i._1 != 0).get
      val moves = 2 * (f._1 - 1) - 1
      moves + resolve(items.updated(f._2 + 1, items(f._2 + 1) + f._1).updated(f._2, 0))
    }

  override def part1(input: List[Int]): Int = resolve(input)

  override def part2(input: List[Int]): Int = resolve(input.updated(0, input.head + 4))
}