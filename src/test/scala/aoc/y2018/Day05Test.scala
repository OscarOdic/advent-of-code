package aoc.y2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  val testInput = "dabAcCaCBAcCcaDA"

  "Day05 - 2018" should "solve first part" in {
    Day05.part1(testInput) should equal(10)

    Day05.part1(Day05.input) should equal(9900)
  }

  it should "solve second part" in {
    Day05.part2(testInput) should equal(4)

    Day05.part2(Day05.input) should equal(4992)
  }
}
