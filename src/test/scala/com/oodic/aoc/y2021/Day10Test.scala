package com.oodic.aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  )

  "Day10 - 2021" should "solve first part" in {
    Day10.part1(testInput) should equal(26397)

    Day10.part1(Day10.input) should equal(323691)
  }

  it should "solve second part" in {
    Day10.part2(testInput) should equal(288957)

    Day10.part2(Day10.input) should equal(2858785164L)
  }
}
