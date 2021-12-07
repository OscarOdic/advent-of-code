package com.oodic.aoc

import scala.io.Source

trait Puzzle[I,P1,P2] {
  val year: String

  val day: String = this.getClass.getSimpleName.takeRight(3).take(2)

  val input: I

  def getInputFile: List[String] = Source.fromResource(s"y$year/day$day.txt").getLines.toList

  def resolveFirst(input: I): P1
  def resolveSecond(input: I): P2

  def main(args: Array[String]): Unit = {
    val t0 = System.nanoTime
    val part1 = resolveFirst(input)
    val t1 = System.nanoTime

    println(s"[1st star] $part1 (${(t1 - t0)/1000000}ms)")

    val part2 = resolveSecond(input)
    val t2 = System.nanoTime()

    println(s"[2nd star] $part2 (${(t2 - t1)/1000000}ms)")
  }
}
