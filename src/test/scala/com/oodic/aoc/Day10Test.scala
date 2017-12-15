package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day10Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day10.resolveFirstWithSize("3,4,1,5", 5) should be(12)

    Day10.resolveFirst(Day10.input) should be(1935)
  }

  it should "answer second part" in {
    Day10.resolveSecond("AoC 2017") should be("33efeb34ea91902bb2f59c9920caa6cd")
    Day10.resolveSecond("1,2,3") should be("3efbe78a8d82f29979031a4aa0b16a9d")
    Day10.resolveSecond("1,2,4") should be("63960835bcdc130f0b66d7ff4f6a5a8e")

    Day10.resolveSecond(Day10.input) should be("dc7e7dee710d4c7201ce42713e6b8359")
  }
}
