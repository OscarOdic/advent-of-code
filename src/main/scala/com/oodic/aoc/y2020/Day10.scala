package com.oodic.aoc.y2020

import scala.annotation.tailrec

object Day10 extends Puzzle2020[List[Int], Int, Long] {
  override val input: List[Int] =
    getInputFile.map(_.toInt)

  @tailrec
  private def getDifferencesWithAllAdapters(input: List[Int], jolt: Int = 0, differencesJolt1: Int = 0, differencesJolt3: Int = 0): List[Int] =
    input match {
      case h :: t if h - jolt == 1 => getDifferencesWithAllAdapters(t, h, differencesJolt1 + 1, differencesJolt3)
      case h :: t if h - jolt == 3 => getDifferencesWithAllAdapters(t, h, differencesJolt1, differencesJolt3 + 1)
      case h :: t => getDifferencesWithAllAdapters(t, h, differencesJolt1, differencesJolt3)
      case _ => List(differencesJolt1, differencesJolt3 + 1)
    }

  private def getAllWays(input: List[Int], jolt: Int = 0, savedResult: Map[Int, Long] = Map.empty): (Long, Map[Int, Long]) = {
    savedResult.get(jolt) match {
      case Some(value) => (value, savedResult)
      case _ => input match {
        case h1 :: h2 :: h3 :: t =>
          val (ways1, map1) = getAllWays(h2 :: h3 :: t, h1, savedResult)
          val (ways2, map2) = if (h2 - jolt <= 3) getAllWays(h3 :: t, h2, map1) else (0L, map1)
          val (ways3, map3) = if (h3 - jolt <= 3) getAllWays(t, h3, map2) else (0L, map2)
          val total = ways1 + ways2 + ways3
          (total, map3.updated(jolt, total))
        case h1 :: h2 :: t =>
          val (ways1, map1) = getAllWays(h2 :: t, h1, savedResult)
          val (ways2, map2) = if (h2 - jolt <= 3) getAllWays(t, h2, map1) else (0L, map1)
          val total = ways1 + ways2
          (total, map2.updated(jolt, total))
        case h :: t =>
          val (ways, map) = getAllWays(t, h, savedResult)
          (ways, map.updated(jolt, ways))
        case Nil =>
          (1, savedResult.updated(jolt, 1L))
      }
    }
  }


  override def resolveFirst(input: List[Int]): Int =
    getDifferencesWithAllAdapters(input.sorted).product

  override def resolveSecond(input: List[Int]): Long =
    getAllWays(input.sorted)._1
}
