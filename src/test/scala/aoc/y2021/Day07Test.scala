package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day07Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    16,1,2,0,4,2,7,1,2,14
  )

  "Day07 - 2021" should "solve first part" in {
    Day07.part1(testInput) should equal(37)

    Day07.part1(Day07.input) should equal(344535)
  }

  it should "solve second part" in {
    Day07.part2(testInput) should equal(168)

    Day07.part2(Day07.input) should equal(95581659)
  }
}
