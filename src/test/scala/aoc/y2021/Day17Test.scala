package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Test extends AnyFlatSpec with Matchers {
  val testInput = ((20, 30), (-10, -5))

  "Day17 - 2021" should "solve first part" in {
    Day17.part1(testInput) should equal(45)

    Day17.part1(Day17.input) should equal(6555)
  }

  it should "solve second part" in {
    Day17.part2(testInput) should equal(112)

    Day17.part2(Day17.input) should equal(4973)
  }
}
