package aoc.y2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2"
  )

  "Day03 - 2018" should "solve first part" in {
    Day03.part1(testInput) should equal(4)

    Day03.part1(Day03.input) should equal(112418)
  }

  it should "solve second part" in {
    Day03.part2(testInput) should equal(3)

    Day03.part2(Day03.input) should equal(560)
  }
}
