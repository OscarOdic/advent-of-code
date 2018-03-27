package com.oodic.aoc.y2016

object Day02 extends Puzzle2016[List[String], String, String] {
  override val input: List[String] = getInputFile

  private def getPosition(initialPosition: (Int, Int), direction: Char) =
    direction match {
      case 'U' => initialPosition.copy(_2 = initialPosition._2 - 1)
      case 'R' => initialPosition.copy(_1 = initialPosition._1 + 1)
      case 'L' => initialPosition.copy(_1 = initialPosition._1 - 1)
      case 'D' => initialPosition.copy(_2 = initialPosition._2 + 1)
    }

  private def resolve(keyboard: Map[(Int, Int), Char], input: List[String], position: (Int, Int) = (0, 0)) =
    input.foldLeft((position, "")) {
      case ((initialPosition, code), line) =>
        val newPosition = line.foldLeft(initialPosition) { (currentPosition, direction) =>
          val newPosition = getPosition(currentPosition, direction)
          if (keyboard.keys.toVector.contains(newPosition)) newPosition
          else currentPosition
        }
        (newPosition, code + keyboard(newPosition))
    }._2

  override def resolveFirst(input: List[String]): String = {
    val keyboard = Map(
      (-1, -1) -> '1',
      (0, -1) -> '2',
      (1, -1) -> '3',
      (-1, 0) -> '4',
      (0, 0) -> '5',
      (1, 0) -> '6',
      (-1, 1) -> '7',
      (0, 1) -> '8',
      (1, 1) -> '9'
    )
    resolve(keyboard, input)
  }
  override def resolveSecond(input: List[String]): String = {
    val keyboard = Map(
      (2, -2) -> '1',
      (1, -1) -> '2',
      (2, -1) -> '3',
      (3, -1) -> '4',
      (0, 0) -> '5',
      (1, 0) -> '6',
      (2, 0) -> '7',
      (3, 0) -> '8',
      (4, 0) -> '9',
      (1, 1) -> 'A',
      (2, 1) -> 'B',
      (3, 1) -> 'C',
      (2, 2) -> 'D'
    )
    resolve(keyboard, input)
  }
}