package aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  private val testInput = List(
    List(
        1000,
        2000,
        3000
      ),
      List(
        4000
      ),
      List(
        5000,
        6000
      ),
      List(
        7000,
        8000,
        9000
      ),
      List(
        10000
      )
    )

  "Day01 - 2022" should "solve first part" in {
    Day01.part1(testInput) should equal(24000)

    Day01.part1(Day01.input) should equal(69693)
  }

  it should "solve second part" in {
    Day01.part2(testInput) should equal(45000)

    Day01.part2(Day01.input) should equal(200945)
  }
}
