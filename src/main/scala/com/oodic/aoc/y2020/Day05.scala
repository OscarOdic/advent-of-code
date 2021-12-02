package com.oodic.aoc.y2020

import scala.annotation.tailrec

object Day05 extends Puzzle2020[List[List[Char]], Int, Int] {
  override val input: List[List[Char]] =
    getInputFile.map(_.toList)

  @tailrec
  private def getNumber(code: List[Char], left: Char, right: Char, min: Int, max: Int): Int =
    code match {
      case h :: t if h == left => getNumber(t, left, right, min, max - ((max - min + 1) / 2))
      case h :: t if h == right => getNumber(t, left, right, min + ((max - min + 1) / 2), max)
      case _ => min
    }

  private def codeToPlace(code: List[Char]): (Int, Int) =
    (
      getNumber(code.take(7), 'F', 'B', 0, 127),
      getNumber(code.takeRight(3), 'L', 'R', 0, 7)
    )

  private def getAllIds(codes: List[List[Char]]): List[Int] =
    codes
      .map(codeToPlace)
      .map(pos => pos._1 * 8 + pos._2)

  override def resolveFirst(input: List[List[Char]]): Int =
    getAllIds(input)
      .max

  override def resolveSecond(input: List[List[Char]]): Int =
    getAllIds(input)
      .sorted
      .sliding(2)
      .find { case left :: right :: _ => right == left + 2 }
      .map(_.head + 1)
      .getOrElse(0)
}
