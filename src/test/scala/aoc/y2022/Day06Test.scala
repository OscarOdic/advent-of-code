package aoc.y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Test extends AnyFlatSpec with Matchers {
  private val testInput1 = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  private val testInput2 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
  private val testInput3 = "nppdvjthqldpwncqszvftbrmjlhg"
  private val testInput4 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
  private val testInput5 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

  "Day06 - 2022" should "solve first part" in {
    Day06.part1(testInput1) should equal(7)
    Day06.part1(testInput2) should equal(5)
    Day06.part1(testInput3) should equal(6)
    Day06.part1(testInput4) should equal(10)
    Day06.part1(testInput5) should equal(11)

    Day06.part1(Day06.input) should equal(1566)
  }

  it should "solve second part" in {
    Day06.part2(testInput1) should equal(19)
    Day06.part2(testInput2) should equal(23)
    Day06.part2(testInput3) should equal(23)
    Day06.part2(testInput4) should equal(29)
    Day06.part2(testInput5) should equal(26)

    Day06.part2(Day06.input) should equal(2265)
  }
}
