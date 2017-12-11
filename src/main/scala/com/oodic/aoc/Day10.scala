package com.oodic.aoc

import scala.io.Source

object Day10 {
  val input = Source.fromResource("day10.txt").getLines.mkString

  def tieKnot(size: Int, lengths: List[Int], rounds: Int = 1) = {
    def rec(list: List[Int], lengths: List[Int], position: Int = 0, skip: Int = 0): (List[Int], Int, Int) = lengths match {
      case Nil => (list, position, skip)
      case h::t =>
        val reversedSection = (position until position + h)
          .map(pos => list(pos % list.size))
          .reverse
        val newList = (position until position + h)
          .foldLeft(list)((listToReverse, pos) =>
            listToReverse.updated(
              pos % listToReverse.size,
              reversedSection(pos-position)
            )
          )
        val newPosition = (position + h + skip) % newList.size
        rec(newList, t, newPosition, skip + 1)
    }

    (0 until rounds).foldLeft(((0 until size).toList, 0, 0)) {
      case ((list, position, skip), _) => rec(list, lengths, position, skip)
    }._1
  }

  def getHash(sparseHash: List[Int]) =
    (0 until 16).map(blockNr =>
      sparseHash.slice(blockNr * 16, (blockNr + 1) * 16).reduce(_ ^ _)
    ).toList

  def getHex(denseHash: List[Int]) = denseHash
    .map(digit =>
      if (digit.toHexString.length == 1) "0" + digit.toHexString
      else digit.toHexString
    ).mkString("")

  def resolveFirst(input: String, size: Int = 256) =
    tieKnot(size, input.split(",").map(_.toInt).toList).take(2).product

  def resolveSecond(input: String, size: Int = 256) = {
    val lengths = input.split("").toList.map(_.codePointAt(0)) ++ List(17, 31, 73, 47, 23)
    getHex(getHash(tieKnot(size, lengths, 64)))
  }

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
