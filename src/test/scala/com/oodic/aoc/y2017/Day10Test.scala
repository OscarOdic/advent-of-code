package com.oodic.aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {
  "Day10 - 2017" should "solve first part" in {
    Day10.resolveFirstWithSize("3,4,1,5", 5) should equal(12)

    Day10.part1(Day10.input) should equal(1935)
  }

  it should "solve second part" in {
    Day10.part2("AoC 2017") should equal("33efeb34ea91902bb2f59c9920caa6cd")
    Day10.part2("1,2,3") should equal("3efbe78a8d82f29979031a4aa0b16a9d")
    Day10.part2("1,2,4") should equal("63960835bcdc130f0b66d7ff4f6a5a8e")

    Day10.part2(Day10.input) should equal("dc7e7dee710d4c7201ce42713e6b8359")
  }
}
