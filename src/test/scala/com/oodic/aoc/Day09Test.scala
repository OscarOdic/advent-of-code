package com.oodic.aoc

import org.scalatest.{FlatSpec, Matchers}

class Day09Test extends FlatSpec with Matchers {
  it should "answer first part" in {
    Day09.resolveFirst("{}") should be(1)
    Day09.resolveFirst("{{{}}}") should be(6)
    Day09.resolveFirst("{{},{}}") should be(5)
    Day09.resolveFirst("{{{},{},{{}}}}") should be(16)
    Day09.resolveFirst("{<a>,<a>,<a>,<a>}") should be(1)
    Day09.resolveFirst("{{<ab>},{<ab>},{<ab>},{<ab>}}") should be(9)
    Day09.resolveFirst("{{<!!>},{<!!>},{<!!>},{<!!>}}") should be(9)
    Day09.resolveFirst("{{<a!>},{<a!>},{<a!>},{<ab>}}") should be(3)

    Day09.resolveFirst(Day09.input) should be(9251)
  }

  it should "answer second part" in {
    Day09.resolveSecond("{<>}") should be(0)
    Day09.resolveSecond("{<<<<>}") should be(3)
    Day09.resolveSecond("{<{!>}>}") should be(2)
    Day09.resolveSecond("{<!!>}") should be(0)
    Day09.resolveSecond("{<!!!>>}") should be(0)
    Day09.resolveSecond("{<{o\"i!a,<{i<a>}") should be(10)

    Day09.resolveSecond(Day09.input) should be(4322)
  }
}
