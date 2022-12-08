package aoc.y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers {
  "Day03 - 2019" should "solve first part" in {
    Day03.part1(Vector(List("R8","U5","L5","D3"), List("U7","R6","D4","L4"))) should equal(6)
    Day03.part1(Vector(List("R75","D30","R83","U83","L12","D49","R71","U7","L72"), List("U62","R66","U55","R34","D71","R55","D58","R83"))) should equal(159)
    Day03.part1(Vector(List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"), List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"))) should equal(135)
    Day03.part1(Day03.input) should equal(4981)
  }

  it should "solve second part" in {
    Day03.part2(Vector(List("R8","U5","L5","D3"), List("U7","R6","D4","L4"))) should equal(30)
    Day03.part2(Vector(List("R75","D30","R83","U83","L12","D49","R71","U7","L72"), List("U62","R66","U55","R34","D71","R55","D58","R83"))) should equal(610)
    Day03.part2(Vector(List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"), List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"))) should equal(410)
    Day03.part2(Day03.input) should equal(164012)
  }
}
