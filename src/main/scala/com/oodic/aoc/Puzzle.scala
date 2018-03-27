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
    println(s"[1st star] ${resolveFirst(input)}")
    println(s"[2nd star] ${resolveSecond(input)}")
  }
}
