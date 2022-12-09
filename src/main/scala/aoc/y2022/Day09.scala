package aoc.y2022

import scala.annotation.tailrec

object Day09 extends Puzzle2022[List[String], Int, Int] {
  override val input: List[String] = getInputFile

  private def isTouching(headPosition: (Int, Int), tailPosition: (Int, Int)): Boolean =
    (for {
      x <- headPosition._1 - 1 to headPosition._1 + 1
      y <- headPosition._2 - 1 to headPosition._2 + 1
    } yield (x, y))
      .contains(tailPosition)

  private def getTailPosition(headPosition: (Int, Int), tailInitialPosition: (Int, Int)): (Int, Int) =
    if (isTouching(headPosition, tailInitialPosition))
      tailInitialPosition
    else {
      val (xH, yH) = headPosition
      val (xT, yT) = tailInitialPosition
      if (xT == xH + 2 && yT == yH + 2) (xH + 1, yH + 1)
      else if (xT == xH + 2 && yT == yH - 2) (xH + 1, yH - 1)
      else if (xT == xH - 2 && yT == yH + 2) (xH - 1, yH + 1)
      else if (xT == xH - 2 && yT == yH - 2) (xH - 1, yH - 1)
      else if (yT == yH - 2) (xH, yH - 1)
      else if (yT == yH + 2) (xH, yH + 1)
      else if (xT == xH - 2) (xH - 1, yH)
      else if (xT == xH + 2) (xH + 1, yH)
      else tailInitialPosition
    }

  @tailrec
  private def executeOperation(
                                direction: String,
                                length: Int,
                                headPosition: (Int, Int),
                                tailsPositions: List[List[(Int, Int)]]
                              ): ((Int, Int), List[List[(Int, Int)]]) =
    if (length == 0)
      (headPosition, tailsPositions)
    else {
      val newHeadPosition = direction match
        case "U" => headPosition.copy(_2 = headPosition._2 - 1)
        case "D" => headPosition.copy(_2 = headPosition._2 + 1)
        case "R" => headPosition.copy(_1 = headPosition._1 + 1)
        case "L" => headPosition.copy(_1 = headPosition._1 - 1)

      val newTailsPositions = tailsPositions.foldLeft((newHeadPosition, List.empty[List[(Int, Int)]])) {
        case ((precedentPosition, accTailsPositions), tailPositions) =>
          val newTailPosition = getTailPosition(precedentPosition, tailPositions.head)
          (newTailPosition, accTailsPositions :+ (newTailPosition +: tailPositions))
      }._2
      executeOperation(direction, length - 1, newHeadPosition, newTailsPositions)
    }

  private def executeInput(input: List[String], nbTails: Int): Set[(Int, Int)] = {
    val initialPosition = (0, 0)
    val initialTailsPositions = (1 to nbTails).map(_ => List((0, 0))).toList
    input.foldLeft((initialPosition, initialTailsPositions)) {
      case ((headPosition, tailsPositions), operation) =>
        operation.split(" ") match
          case Array(direction, length) => executeOperation(direction, length.toInt, headPosition, tailsPositions)
    }._2.last.toSet
  }

  override def part1(input: List[String]): Int =
    executeInput(input, 1)
      .size

  override def part2(input: List[String]): Int =
    executeInput(input, 9)
      .size
}
