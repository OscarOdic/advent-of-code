package com.oodic.aoc.y2019

object Day04 extends Puzzle2019[String, Int, Int] {
  override  val input: String = getInputFile.head

  private def isPassword(value: String): Boolean = {
    val zippedValue = value.zipWithIndex
    zippedValue.forall {case (char, i) => (i == zippedValue.size - 1) || (char <= zippedValue(i+1)._1)} &&
      zippedValue.exists {case (char, i) => (i < zippedValue.size - 1) && (char == zippedValue(i+1)._1)}
  }

  private def isMoreRestrictivePassword(value: String): Boolean = {
    val zippedValue = value.zipWithIndex
    zippedValue.forall {case (char, i) => (i == zippedValue.size - 1) || (char <= zippedValue(i+1)._1)} &&
      zippedValue.exists {case (char, i) => (i < zippedValue.size - 1) && (char == zippedValue(i+1)._1) &&
        (if (i < zippedValue.size - 2) char != zippedValue(i+2)._1 else true) && (if (i > 0) char != zippedValue(i-1)._1 else true)
      }
  }

  override def resolveFirst(input:  String): Int = input.split("-").map(_.toInt) match {
    case Array(min, max) => (min to max).map(_.toString).count(isPassword)
  }

  override def resolveSecond(input:  String): Int = input.split("-").map(_.toInt) match {
    case Array(min, max) => (min to max).map(_.toString).count(isMoreRestrictivePassword)
  }
}
