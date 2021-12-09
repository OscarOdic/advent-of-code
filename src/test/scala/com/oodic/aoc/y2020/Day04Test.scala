package com.oodic.aoc.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    List("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
      "byr:1937 iyr:2017 cid:147 hgt:183cm"),

    List("iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
      "hcl:#cfa07d byr:1929"),

    List("hcl:#ae17e1 iyr:2013",
      "eyr:2024",
      "ecl:brn pid:760753108 byr:1931",
      "hgt:179cm"),

    List("hcl:#cfa07d eyr:2025 pid:166559648",
      "iyr:2011 ecl:brn hgt:59in")
  )

  "Day04 - 2020" should "solve first part" in {
    Day04.part1(testInput) should equal(2)

    Day04.part1(Day04.input) should equal(233)
  }

  it should "solve second part" in {
    Day04.part2(Day04.input) should equal(111)
  }
}
