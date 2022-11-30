package com.oodic.aoc.y2021

object Day12 extends Puzzle2021[List[String], Int, Int] {
  override val input: List[String] =
    getInputFile

  private def parseCaveTuple(caveTuple: String): List[(String, String)] =
    caveTuple
      .split("-") match {
        case Array("start", right) => List("start" -> right)
        case Array(left, "start") => List("start" -> left)
        case Array(left, "end") => List(left -> "end")
        case Array("end", right) => List(right -> "end")
        case Array(left, right) => List(left -> right, right -> left)
      }

  private def parseCaves(paths: List[String]): Map[String, List[String]] =
    paths
      .flatMap(parseCaveTuple)
      .groupBy(_._1)
      .view.mapValues(_.map(_._2)).toMap

  private def isLargeCave(cave: String) =
    cave.head.isUpper

  private def allPathsFirstPart(caves: Map[String, List[String]], position: String = "start", smallCavesAlreadyVisited: List[String] = List.empty): Int =
    if (position == "end") 1
    else
      caves
        .getOrElse(position, List.empty)
        .filter(!smallCavesAlreadyVisited.contains(_)) match {
          case Nil => 0
          case list =>
            list.map(cave =>
              allPathsFirstPart(caves, cave, if (isLargeCave(cave)) smallCavesAlreadyVisited else cave +: smallCavesAlreadyVisited)
            ).sum
        }

  private def allPathsSecondPart(caves: Map[String, List[String]], position: String = "start", smallCavesAlreadyVisitedOnce: List[String] = Nil, smallCavesAlreadyVisitedTwice: Option[String] = None): Int =
    if (position == "end") 1
    else
      caves
        .getOrElse(position, List.empty)
        .filter(cave =>
          smallCavesAlreadyVisitedTwice.isEmpty ||
            (!smallCavesAlreadyVisitedTwice.contains(cave) && !smallCavesAlreadyVisitedOnce.contains(cave))
        ) match {
        case Nil => 0
        case list =>
          list.map { cave =>
            val alreadyVisitedOnce = smallCavesAlreadyVisitedOnce.contains(cave)
            allPathsSecondPart(
              caves,
              cave,
              if (isLargeCave(cave) || alreadyVisitedOnce) smallCavesAlreadyVisitedOnce else cave +: smallCavesAlreadyVisitedOnce,
              if (!isLargeCave(cave) && alreadyVisitedOnce) Some(cave) else smallCavesAlreadyVisitedTwice
            )
          }.sum
      }


  override def part1(input: List[String]): Int =
    allPathsFirstPart(parseCaves(input))

  override def part2(input: List[String]): Int =
    allPathsSecondPart(parseCaves(input))
}
