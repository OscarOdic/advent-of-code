package com.oodic.aoc.y2016

import org.scalatest.{FlatSpec, Matchers}

class Day17Test extends FlatSpec with Matchers {
  "Day17 - 2016" should "answer first part" in {
    Day17.resolveFirst("ihgpwlah") should be("DDRRRD")
    Day17.resolveFirst("kglvqrro") should be("DDUDRLRRUDRD")
    Day17.resolveFirst("ulqzkmiv") should be("DRURDRUDDLLDLUURRDULRLDUUDDDRR")

    Day17.resolveFirst(Day17.input) should be("RLRDRDUDDR")
  }

  it should "answer second part" in {
    Day17.resolveSecond("ihgpwlah") should be(370)
    Day17.resolveSecond("kglvqrro") should be(492)
    Day17.resolveSecond("ulqzkmiv") should be(830)

    Day17.resolveSecond(Day17.input) should be(420)
  }
}
