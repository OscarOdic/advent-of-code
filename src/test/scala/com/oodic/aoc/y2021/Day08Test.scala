package com.oodic.aoc.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Test extends AnyFlatSpec with Matchers {
  val testInput = List(
    (List("be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"), List("fdgacbe","cefdb","cefbgd","gcbe")),
    (List("edbfga","begcd","cbg","gc","gcadebf","fbgde","acbgfd","abcde","gfcbed","gfec"), List("fcgedb","cgb","dgebacf","gc")),
    (List("fgaebd","cg","bdaec","gdafb","agbcfd","gdcbef","bgcad","gfac","gcb","cdgabef"), List("cg","cg","fdcagb","cbg")),
    (List("fbegcd","cbd","adcefb","dageb","afcb","bc","aefdc","ecdab","fgdeca","fcdbega"), List("efabcd","cedba","gadfec","cb")),
    (List("aecbfdg","fbg","gf","bafeg","dbefa","fcge","gcbea","fcaegb","dgceab","fcbdga"), List("gecf","egdcabf","bgf","bfgea")),
    (List("fgeab","ca","afcebg","bdacfeg","cfaedg","gcfdb","baec","bfadeg","bafgc","acf"), List("gebdcfa","ecba","ca","fadegcb")),
    (List("dbcfg","fgd","bdegcaf","fgec","aegbdf","ecdfab","fbedc","dacgb","gdcebf","gf"), List("cefg","dcbef","fcge","gbcadfe")),
    (List("bdfegc","cbegaf","gecbf","dfcage","bdacg","ed","bedf","ced","adcbefg","gebcd"), List("ed","bcgafe","cdgba","cbgef")),
    (List("egadfb","cdbfeg","cegd","fecab","cgb","gbdefca","cg","fgcdab","egfdb","bfceg"), List("gbdfcae","bgc","cg","cgb")),
    (List("gcafb","gcf","dcaebfg","ecagb","gf","abcdeg","gaef","cafbge","fdbac","fegbdc"), List("fgae","cfgab","fg","bagce"))
  )

  "Day08 - 2021" should "solve first part" in {
    Day08.part1(testInput) should equal(26)

    Day08.part1(Day08.input) should equal(470)
  }

  it should "solve second part" in {
    Day08.part2(testInput) should equal(61229)

    Day08.part2(Day08.input) should equal(989396)
  }
}
