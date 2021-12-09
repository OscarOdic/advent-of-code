package com.oodic.aoc

import scala.io.Source

trait Puzzle[I,P1,P2] {
  val year: String

  val day: String = this.getClass.getSimpleName.takeRight(3).take(2)

  val input: I

  def getInputFile: List[String] = Source.fromResource(s"y$year/day$day.txt").getLines.toList

  def part1(input: I): P1
  def part2(input: I): P2

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime
    val p1 = part1(input)
    val t1 = System.nanoTime

    println(s"[1st star] $p1 (${(t1 - t0)/1000000}ms)")

    val p2 = part2(input)
    val t2 = System.nanoTime()

    println(s"[2nd star] $p2 (${(t2 - t1)/1000000}ms)")
  }
}
