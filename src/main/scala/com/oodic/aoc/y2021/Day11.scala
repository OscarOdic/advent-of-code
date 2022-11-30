package com.oodic.aoc.y2021

import scala.annotation.tailrec

object Day11 extends Puzzle2021[Map[(Int, Int), Int], Int, Int] {
  override val input: Map[(Int, Int), Int] =
    (for {
      (line, y) <- getInputFile.zipWithIndex
      (value, x) <- line.zipWithIndex
    } yield (x, y) -> value.asDigit).toMap

  private def neighbors(x: Int, y: Int): Seq[(Int, Int)] =
    for {
      xNeighbor <- x - 1 to x + 1
      yNeighbor <- y - 1 to y + 1
      if xNeighbor != x || yNeighbor != y
    } yield (xNeighbor, yNeighbor)

  private def resetFlashed(octopusMap: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
    octopusMap.view.mapValues(value =>
      if (value == 10) 0 else value
    ).toMap

  private def flash(x: Int, y: Int, octopusMap: Map[(Int, Int), Int], nbFlash: Int = 1): (Map[(Int, Int), Int], Int) =
    neighbors(x, y).foldLeft((octopusMap, nbFlash)) {
      case ((currentMap, currentNbFlash), (xNeighbor, yNeighbor)) =>
        currentMap.get(xNeighbor, yNeighbor) match {
          case Some(10) | None =>
            (currentMap, currentNbFlash)
          case Some(9) =>
            flash(xNeighbor, yNeighbor, currentMap.updated((xNeighbor, yNeighbor), 10), currentNbFlash + 1)
          case Some(v) =>
            (currentMap.updated((xNeighbor, yNeighbor), v + 1), currentNbFlash)
        }
    }

  private def executeStep(octopusMap: Map[(Int, Int), Int], nbFLash: Int = 0): (Map[(Int, Int), Int], Int) = {
    val increasedOctopusMap = octopusMap.view.mapValues(_ + 1).toMap
    increasedOctopusMap.filter(_._2 == 10).keys
      .foldLeft((increasedOctopusMap, nbFLash)) {
        case ((currentMap, currentNbFlash), (x, y)) =>
          flash(x, y, currentMap, currentNbFlash + 1)
      }
  }

  @tailrec
  private def executeSteps(octopusMap: Map[(Int, Int), Int], nbSteps: Int = 100, nbFLash: Int = 0): (Map[(Int, Int), Int], Int) =
    if (nbSteps == 0) (octopusMap, nbFLash)
    else
      executeStep(octopusMap, nbFLash) match {
        case (nextOctopusMap, nextNbFlash) =>
          executeSteps(resetFlashed(nextOctopusMap), nbSteps - 1, nextNbFlash)
      }

  @tailrec
  private def getStepAllFlash(octopusMap: Map[(Int, Int), Int], step: Int = 0): Int =
    if (octopusMap.forall(_._2 == 0))
      step
    else {
      getStepAllFlash(resetFlashed(executeStep(octopusMap)._1), step + 1)
    }

  override def part1(input: Map[(Int, Int), Int]): Int =
    executeSteps(input)._2

  override def part2(input: Map[(Int, Int), Int]): Int =
    getStepAllFlash(input)
}
