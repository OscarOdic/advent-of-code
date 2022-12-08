package aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
  val testInput: List[Long] = List(
    3,4,3,1,2
  )

  "Day06 - 2021" should "solve first part" in {
    Day06.part1(testInput) should equal(5934)

    Day06.part1(Day06.input) should equal(371379)
  }

  it should "solve second part" in {
    Day06.part2(testInput) should equal(26984457539L)

    Day06.part2(Day06.input) should equal(1674303997472L)
  }
}
