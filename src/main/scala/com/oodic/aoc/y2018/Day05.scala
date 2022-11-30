package com.oodic.aoc.y2018

import scala.collection.parallel.CollectionConverters._

object Day05 extends Puzzle2018[String, Int, Int] {
  override val input: String = getInputFile.head

  private val tokens = {
    val tokensTmp = ('a' to 'z').map(letter => s"$letter${letter.toUpper}")
    tokensTmp ++ tokensTmp.map(_.reverse)
  }

  private val lettersToRemove =
    tokens.map(_.toSeq.map(_.toString))

  private def reductPolymers(polymers: String): String = {
    val reducted = tokens.foldLeft(polymers)((reductedPolymers, token) => reductedPolymers.replace(token, ""))
    if (reducted == polymers) reducted
    else reductPolymers(reducted)
  }

  override def part1(input: String): Int = reductPolymers(input).length

  override def part2(input: String): Int =
    lettersToRemove.par
      .map(letters => reductPolymers(letters.foldLeft(input)((polymers, letter) => polymers.replace(letter, ""))).length)
      .min
}
