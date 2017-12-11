package com.oodic.aoc

import scala.io.Source

object Day4 {
  val input = Source.fromResource("day4.txt").getLines.toList

  def resolveWithF(passwords: Seq[String])(f: Seq[String] => Boolean) = passwords.count(password => {
    f(password.split(" "))
  })

  def resolveFirst(passwords: List[String]) =
    resolveWithF(passwords)(words => words.distinct equals words)

  def resolveSecond(passwords: List[String]) =
    resolveWithF(passwords)(words => {
      val sortedWords = words.map(_.sorted)
      sortedWords.distinct equals sortedWords
    })

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
