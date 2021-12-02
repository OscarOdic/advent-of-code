package com.oodic.aoc.y2020

import scala.annotation.tailrec

object Day09 extends Puzzle2020[List[Long], Long, Long] {
  override val input: List[Long] =
    getInputFile.map(_.toLong)

  @tailrec
  private def validate(n: Long, preambleRight: List[Long], preambleLeft: List[Long] = List.empty[Long]): Boolean =
    preambleRight match {
      case h :: t if (preambleLeft ++ t).exists(_ + h == n) => true
      case h :: t => validate(n, t, preambleLeft :+ h)
      case _ => false
    }

  @tailrec
  private def firstInvalidate(preamble: List[Long], values: List[Long]): Long =
    values match {
      case h :: t if validate(h, preamble) =>
        firstInvalidate(preamble.tail :+ h, t)
      case h :: _ => h
      case _ => 0
    }

  private def contiguousSet(number: Long, values: List[Long], combinations: Int = 2): List[Long] =
    values
      .sliding(combinations)
      .find(_.sum == number) match {
      case Some(value) => value
      case _ => contiguousSet(number, values, combinations + 1)
    }


  override def resolveFirst(input: List[Long]): Long = {
    val (preamble, next) = input.splitAt(25)
    firstInvalidate(preamble, next)
  }

  override def resolveSecond(input: List[Long]): Long = {
    val invalidNumber = resolveFirst(input)
    val set = contiguousSet(invalidNumber, input.filter(_ != invalidNumber))
    set.min + set.max
  }
}
