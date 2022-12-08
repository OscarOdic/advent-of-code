package aoc.y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day25Test extends AnyFlatSpec with Matchers {
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

  "Day25 - 2017" should "solve first part" in {
    Day25.part1(testInput) should equal(3)

    Day25.part1(Day25.input) should equal(3362)
  }
}