package com.oodic.aoc.y2016

import java.security.MessageDigest

object Day14 extends Puzzle2016[String, Int, Int] {
  override val input: String = "ngcjuoqr"

  private def hash(value: String) =
    MessageDigest.getInstance("MD5").digest(value.getBytes).map("%02x".format(_)).mkString

  private def nMd5(value: String, n: Int) = (0 until n).foldLeft(value) {
    case (v, _) => hash(v).toLowerCase
  }

  private def firstTriple(value: String): Option[String] =
    value.sliding(3).find(_.distinct.length == 1).map(_.head.toString)

  private def getIndexFromKey(salt: String, n: Int = 1): Stream[Int] = {
    val hashes = Stream.iterate(0)(_ + 1).map(value => nMd5(salt + value, n))
    hashes.zipWithIndex.filter {
      case (hash, index) =>
        firstTriple(hash).exists(char => hashes.slice(index + 1, index + 1001).exists(_.contains(char * 5)))
    }.map(_._2)
  }

  override def resolveFirst(input: String): Int = getIndexFromKey(input)(63)

  override def resolveSecond(input: String): Int = getIndexFromKey(input, 2017)(63)
}