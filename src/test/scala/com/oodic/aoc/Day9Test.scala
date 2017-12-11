package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day9Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day9.resolveFirst("{}") should be(1)
    Day9.resolveFirst("{{{}}}") should be(6)
    Day9.resolveFirst("{{},{}}") should be(5)
    Day9.resolveFirst("{{{},{},{{}}}}") should be(16)
    Day9.resolveFirst("{<a>,<a>,<a>,<a>}") should be(1)
    Day9.resolveFirst("{{<ab>},{<ab>},{<ab>},{<ab>}}") should be(9)
    Day9.resolveFirst("{{<!!>},{<!!>},{<!!>},{<!!>}}") should be(9)
    Day9.resolveFirst("{{<a!>},{<a!>},{<a!>},{<ab>}}") should be(3)

    Day9.resolveFirst(Day9.input) should be(9251)
  }

  it should "answer second part" in {
    Day9.resolveSecond("{<>}") should be(0)
    Day9.resolveSecond("{<<<<>}") should be(3)
    Day9.resolveSecond("{<{!>}>}") should be(2)
    Day9.resolveSecond("{<!!>}") should be(0)
    Day9.resolveSecond("{<!!!>>}") should be(0)
    Day9.resolveSecond("{<{o\"i!a,<{i<a>}") should be(10)

    Day9.resolveSecond(Day9.input) should be(4322)
  }
}
