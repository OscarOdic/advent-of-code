package com.oodic.aoc.y2016

import java.security.MessageDigest

object Day05 extends Puzzle2016[String, String, String] {
  override val input: String = "uqwqemis"

  private def hash(value: String) = MessageDigest.getInstance("MD5").digest(value.getBytes).take(4)

  private def isValid(hash: Array[Byte]) = hash(0) == 0 && hash(1) == 0 && hash(2) >= 0 && hash(2) < 16

  private def getChar(id: String, index: Int = 0): (Char, Int) =
    hash(id + index) match {
      case h if isValid(h) => (("%02x" format h(2)).last, index)
      case _ => getChar(id, index + 1)
    }

  private def getCharIndexed(id: String, index: Int = 0): (Char, Int, Int) =
    hash(id + index) match {
      case h if isValid(h) && h(2).toInt < 8 && h(2) >= 0 => (("%02x" format h(3)).head, h(2).toInt, index)
      case _ => getCharIndexed(id, index+1)
    }

  private def getPassword(id: String, size: Int = 8, index: Int = 0): String =
    if (size == 0) ""
    else {
      val char = getChar(id, index)
      char._1.toString + getPassword(id, size-1, char._2+1)
    }

  private def getPasswordIndexed(id: String, index: Int = 0, password: Map[Int, Char] = Map()): Map[Int, Char] =
    if (password.size == 8) password
    else {
      val char = getCharIndexed(id, index)
      getPasswordIndexed(
        id,
        char._3 + 1,
        if (password.isDefinedAt(char._2)) password
        else password.updated(char._2, char._1)
      )
    }

  override def resolveFirst(input: String): String = getPassword(input)

  override def resolveSecond(input: String): String =
    getPasswordIndexed(input).toList.sorted.map(_._2).mkString
}
