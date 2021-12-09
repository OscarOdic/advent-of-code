package com.oodic.aoc.y2017

object Day04 extends Puzzle2017[List[String], Int, Int] {
  override val input: List[String] = getInputFile

  def resolveWithF(passwords: Seq[String])(f: Seq[String] => Boolean): Int = passwords.count(password => {
    f(password.split(" "))
  })

  override def part1(passwords: List[String]): Int =
    resolveWithF(passwords)(words => words.distinct equals words)

  override def part2(passwords: List[String]): Int =
    resolveWithF(passwords)(words => {
      val sortedWords = words.map(_.sorted)
      sortedWords.distinct equals sortedWords
    })
}
