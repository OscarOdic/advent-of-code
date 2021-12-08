package com.oodic.aoc.y2021

import org.scalatest.{FlatSpec, Matchers}

class Day08Test extends FlatSpec with Matchers {
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

  "Day08 - 2021" should "answer first part" in {
    Day08.resolveFirst(testInput) should be(26)

    Day08.resolveFirst(Day08.input) should be(470)
  }

  it should "answer second part" in {
    Day08.resolveSecond(testInput) should be(61229)

    Day08.resolveSecond(Day08.input) should be(989396)
  }
}
