package aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFlatSpec with Matchers {
  val testInput: List[String] = List(
    "aaaaa-bbb-z-y-x-123[abxyz]",
    "a-b-c-d-e-f-g-h-987[abcde]",
    "not-a-real-room-404[oarel]",
    "totally-real-room-200[decoy]"
  )

  "Day04 - 2016" should "solve first part" in {
    Day04.part1(testInput) should equal(1514)

    Day04.part1(Day04.input) should equal(278221)
  }

  it should "solve second part" in {
    Day04.part2(Day04.input) should equal(267)
  }
}
