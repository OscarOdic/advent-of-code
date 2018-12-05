package com.oodic.aoc.y2018

import org.scalatest.{FlatSpec, Matchers}

class Day04Test extends FlatSpec with Matchers {
  val testInput = List(
    "[1518-11-01 00:00] Guard #10 begins shift",
    "[1518-11-01 00:05] falls asleep",
    "[1518-11-01 00:25] wakes up",
    "[1518-11-01 00:30] falls asleep",
    "[1518-11-01 00:55] wakes up",
    "[1518-11-01 23:58] Guard #99 begins shift",
    "[1518-11-02 00:40] falls asleep",
    "[1518-11-02 00:50] wakes up",
    "[1518-11-03 00:05] Guard #10 begins shift",
    "[1518-11-03 00:24] falls asleep",
    "[1518-11-03 00:29] wakes up",
    "[1518-11-04 00:02] Guard #99 begins shift",
    "[1518-11-04 00:36] falls asleep",
    "[1518-11-04 00:46] wakes up",
    "[1518-11-05 00:03] Guard #99 begins shift",
    "[1518-11-05 00:45] falls asleep",
    "[1518-11-05 00:55] wakes up"
  )

  "Day04 - 2018" should "answer first part" in {
    Day04.resolveFirst(testInput) should be(240)

    Day04.resolveFirst(Day04.input) should be(77941)
  }

  it should "answer second part" in {
    Day04.resolveSecond(testInput) should be(4455)

    Day04.resolveSecond(Day04.input) should be(35289)
  }
}
