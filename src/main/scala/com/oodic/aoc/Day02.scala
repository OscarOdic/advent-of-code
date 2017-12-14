package com.oodic.aoc

import scala.io.Source

object Day02 {
  val input = Source.fromResource("day2.txt").getLines.toList.map(_.split("\t").map(_.toInt).toList)

  def resolveWithF(input: List[List[Int]])(f: List[Int] => Int): Int = input.map(f).sum

  def resolveFirst(input: List[List[Int]]): Int = resolveWithF(input) { l =>
    val min = l.min
    val max = l.max
    max - min
  }

  def resolveSecond(input: List[List[Int]]): Int = resolveWithF(input)(l =>
    l.combinations(2).find {
      case List(first, second) => (first % second == 0) || (second % first == 0)
    }.map {
      case List(first, second) => if (first > second) first/second else second/first
    }.getOrElse(0)
  )

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
