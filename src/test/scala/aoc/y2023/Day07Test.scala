package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day07Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    ("32T3K", 765),
    ("T55J5", 684),
    ("KK677", 28),
    ("KTJJT", 220),
    ("QQQJA", 483)
  )

  "Day07 - 2023" should "solve first part" in {
    Day07.part1(testInput) should equal(6440)

    Day07.part1(Day07.input) should equal(248453531)
  }

  it should "solve second part" in {
    Day07.part2(testInput) should equal(5905)

    Day07.part2(Day07.input) should equal(248781813)
  }
}
