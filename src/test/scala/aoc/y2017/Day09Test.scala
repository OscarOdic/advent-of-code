package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Test extends AnyFlatSpec with Matchers {
  "Day09 - 2017" should "solve first part" in {
    Day09.part1("{}") should equal(1)
    Day09.part1("{{{}}}") should equal(6)
    Day09.part1("{{},{}}") should equal(5)
    Day09.part1("{{{},{},{{}}}}") should equal(16)
    Day09.part1("{<a>,<a>,<a>,<a>}") should equal(1)
    Day09.part1("{{<ab>},{<ab>},{<ab>},{<ab>}}") should equal(9)
    Day09.part1("{{<!!>},{<!!>},{<!!>},{<!!>}}") should equal(9)
    Day09.part1("{{<a!>},{<a!>},{<a!>},{<ab>}}") should equal(3)

    Day09.part1(Day09.input) should equal(9251)
  }

  it should "solve second part" in {
    Day09.part2("{<>}") should equal(0)
    Day09.part2("{<<<<>}") should equal(3)
    Day09.part2("{<{!>}>}") should equal(2)
    Day09.part2("{<!!>}") should equal(0)
    Day09.part2("{<!!!>>}") should equal(0)
    Day09.part2("{<{o\"i!a,<{i<a>}") should equal(10)

    Day09.part2(Day09.input) should equal(4322)
  }
}
