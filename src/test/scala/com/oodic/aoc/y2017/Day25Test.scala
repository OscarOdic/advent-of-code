package com.oodic.aoc.y2017

import org.scalatest.{FlatSpec, Matchers}

class Day25Test extends FlatSpec with Matchers {
  val testInput = """Begin in state A.
                    |Perform a diagnostic checksum after 6 steps.
                    |
                    |In state A:
                    |  If the current value is 0:
                    |    - Write the value 1.
                    |    - Move one slot to the right.
                    |    - Continue with state B.
                    |  If the current value is 1:
                    |    - Write the value 0.
                    |    - Move one slot to the left.
                    |    - Continue with state B.
                    |
                    |In state B:
                    |  If the current value is 0:
                    |    - Write the value 1.
                    |    - Move one slot to the left.
                    |    - Continue with state A.
                    |  If the current value is 1:
                    |    - Write the value 1.
                    |    - Move one slot to the right.
                    |    - Continue with state A.""".replace("|", "\n")

  "Day25 - 2017" should "answer first part" in {
    Day25.resolveFirst(testInput) should be(3)

    Day25.resolveFirst(Day25.input) should be(3362)
  }
}