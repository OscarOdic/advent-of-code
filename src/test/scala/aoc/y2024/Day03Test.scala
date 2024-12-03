package aoc.y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  private val testInput = List("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  private val testInput2 = List("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

  "Day03 - 2024" should "solve first part" in {
    Day03.part1(testInput) should equal(161)

    Day03.part1(Day03.input) should equal(192767529)
  }

  it should "solve second part" in {
    Day03.part2(testInput2) should equal(48)

    Day03.part2(Day03.input) should equal(104083373)
  }
}
