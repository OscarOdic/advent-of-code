package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  val testInput: List[String] = List(
    "ULL",
    "RRDDD",
    "LURDL",
    "UUUUD"
  )

  "Day02 - 2016" should "solve first part" in {
    Day02.part1(testInput) should equal("1985")

    Day02.part1(Day02.input) should equal("98575")
  }

  it should "solve second part" in {
    Day02.part2(testInput) should equal("5DB3")

    Day02.part2(Day02.input) should equal("CD8D4")
  }
}
