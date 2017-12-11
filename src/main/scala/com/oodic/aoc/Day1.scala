package com.oodic.aoc

import scala.io.Source

object Day1 {
  val input = Source.fromResource("day1.txt").getLines.mkString.map(_.asDigit)

  def resolveWithN(list: Seq[Int], n: Int): Int = list.zipWithIndex.map {
    case (value, index) if index+n < list.size => (value, list(index+n))
    case (value, index) => (value, list(index + n - list.size))
  }.foldLeft(0) {
    case (acc, (left, right)) if left == right => acc + left
    case (acc, _) => acc
  }

  def resolveFirst(list: Seq[Int]): Int = resolveWithN(list, 1)

  def resolveSecond(list: Seq[Int]): Int = resolveWithN(list, list.size/2)

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
