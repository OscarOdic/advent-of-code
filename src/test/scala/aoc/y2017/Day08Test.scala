package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "b inc 5 if a > 1",
    "a inc 1 if b < 5",
    "c dec -10 if a >= 1",
    "c inc -20 if c == 10"
  )

  "Day08 - 2017" should "solve first part" in {
    Day08.part1(testInput) should equal(1)

    Day08.part1(Day08.input) should equal(5075)
  }

  it should "solve second part" in {
    Day08.part2(testInput) should equal(10)

    Day08.part2(Day08.input) should equal(7310)
  }
}
