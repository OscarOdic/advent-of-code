package com.oodic.aoc.y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Test extends AnyFlatSpec with Matchers {
  "Day17 - 2016" should "solve first part" in {
    Day17.part1("ihgpwlah") should equal("DDRRRD")
    Day17.part1("kglvqrro") should equal("DDUDRLRRUDRD")
    Day17.part1("ulqzkmiv") should equal("DRURDRUDDLLDLUURRDULRLDUUDDDRR")

    Day17.part1(Day17.input) should equal("RLRDRDUDDR")
  }

  it should "solve second part" in {
    Day17.part2("ihgpwlah") should equal(370)
    Day17.part2("kglvqrro") should equal(492)
    Day17.part2("ulqzkmiv") should equal(830)

    Day17.part2(Day17.input) should equal(420)
  }
}
