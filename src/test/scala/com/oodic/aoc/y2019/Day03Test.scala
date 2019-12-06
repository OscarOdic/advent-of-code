package com.oodic.aoc.y2019

import org.scalatest.{FlatSpec, Matchers}

class Day03Test extends FlatSpec with Matchers {
  "Day03 - 2019" should "answer first part" in {
    Day03.resolveFirst(Vector(List("R8","U5","L5","D3"), List("U7","R6","D4","L4"))) should be(6)
    Day03.resolveFirst(Vector(List("R75","D30","R83","U83","L12","D49","R71","U7","L72"), List("U62","R66","U55","R34","D71","R55","D58","R83"))) should be(159)
    Day03.resolveFirst(Vector(List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"), List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"))) should be(135)
    Day03.resolveFirst(Day03.input) should be(4981)
  }

  it should "answer second part" in {
    Day03.resolveSecond(Vector(List("R8","U5","L5","D3"), List("U7","R6","D4","L4"))) should be(30)
    Day03.resolveSecond(Vector(List("R75","D30","R83","U83","L12","D49","R71","U7","L72"), List("U62","R66","U55","R34","D71","R55","D58","R83"))) should be(610)
    Day03.resolveSecond(Vector(List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"), List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"))) should be(410)
    Day03.resolveSecond(Day03.input) should be(164012)
  }
}
