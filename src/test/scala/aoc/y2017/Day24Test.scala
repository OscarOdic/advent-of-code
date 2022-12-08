package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day24Test extends AnyFlatSpec with Matchers {
  val testInput = Vector(
    "0/2",
    "2/2",
    "2/3",
    "3/4",
    "3/5",
    "0/1",
    "10/1",
    "9/10"
  )

  "Day24 - 2017" should "solve first part" in {
    Day24.part1(testInput) should equal(31)

    Day24.part1(Day24.input) should equal(1940)
  }

  it should "solve second part" in {
    Day24.part2(Day24.input) should equal(1928)
  }
}