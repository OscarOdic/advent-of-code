package aoc.y2018

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  "Day02 - 2018" should "solve first part" in {
    val testInput = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )

    Day02.part1(testInput) should equal(12)

    Day02.part1(Day02.input) should equal(8820)
  }

  it should "solve second part" in {
    val testInput = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )

    Day02.part2(testInput) should equal("fgij")

    Day02.part2(Day02.input) should equal("bpacnmglhizqygfsjixtkwudr")
  }
}
