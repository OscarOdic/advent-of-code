package com.oodic.aoc.y2018

object Day02 extends Puzzle2018[List[String], Int, String] {
  override val input: List[String] = getInputFile

  override def part1(input: List[String]): Int = {
    val (two, three) = input.foldLeft((0, 0)){
      case ((two, three), value) =>
        val count = value.groupBy(identity).view.mapValues(_.size).toMap
        (two + (if (count.exists(_._2 == 2)) 1 else 0), three + (if (count.exists(_._2 == 3)) 1 else 0))
    }
    two * three
  }

  override def part2(input: List[String]): String = (for {
    a <- input
    b <- input
    filtered = a.zip(b).filter(t => t._1 == t._2)
    if filtered.length == a.length - 1
  } yield a.intersect(b)).head
}
