package com.oodic.aoc

object Day04 extends PuzzleDay[List[String], Int, Int] {
  override val input: List[String] = getInputFile

  def resolveWithF(passwords: Seq[String])(f: Seq[String] => Boolean): Int = passwords.count(password => {
    f(password.split(" "))
  })

  override def resolveFirst(passwords: List[String]): Int =
    resolveWithF(passwords)(words => words.distinct equals words)

  override def resolveSecond(passwords: List[String]): Int =
    resolveWithF(passwords)(words => {
      val sortedWords = words.map(_.sorted)
      sortedWords.distinct equals sortedWords
    })
}
