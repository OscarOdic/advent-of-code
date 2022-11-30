package com.oodic.aoc.y2016

object Day01 extends Puzzle2016[ List[String], Int, Int] {
  override val input: List[String] = getInputFile.mkString.split(", ").toList

  trait Direction
  object North extends Direction
  object East extends Direction
  object South extends Direction
  object West extends Direction

  case class Instruction(orientation: Char, value: Int)

  private def toInstructions(input: List[String]) = input.map(i => Instruction(i.head, i.tail.toInt))

  private def getDirection(initialDirection: Direction, orientation: Char) = initialDirection match {
    case North if orientation == 'R' => East
    case North if orientation == 'L' => West
    case East if orientation == 'R' => South
    case East if orientation == 'L' => North
    case South if orientation == 'R' => West
    case South if orientation == 'L' => East
    case West if orientation == 'R' => North
    case West if orientation == 'L' => South
  }

  private def getPosition(initialPosition: (Int, Int), direction: Direction, value: Int) = direction match {
    case North => initialPosition.copy(_2 = initialPosition._2 - value)
    case East => initialPosition.copy(_1 = initialPosition._1 + value)
    case South => initialPosition.copy(_2 = initialPosition._2 + value)
    case West => initialPosition.copy(_1 = initialPosition._1 - value)
  }

  private def getHistoryPosition(
                                  initialPosition: (Int, Int),
                                  direction: Direction,
                                  value: Int,
                                  history: Vector[(Int, Int)]
                                ): ((Int, Int), Vector[(Int, Int)]) = value match {
    case 0 => (initialPosition, history)
    case n =>
      val newPosition = getPosition(initialPosition, direction, 1)
      getHistoryPosition(newPosition, direction, n-1, history :+ newPosition)
  }

  private def getDistance(position: (Int, Int)) = math.abs(position._1) + math.abs(position._2)

  override def part1(input: List[String]): Int =
    getDistance(toInstructions(input).foldLeft((North: Direction, (0, 0))) {
      case ((direction, position), instruction) =>
        val newDirection = getDirection(direction, instruction.orientation)
        (newDirection, getPosition(position, newDirection, instruction.value))
    }._2)

  override def part2(input: List[String]): Int = {
    def rec(
             position: (Int, Int) = (0, 0),
             direction: Direction = North,
             instructions: List[Instruction] = toInstructions(input),
             history: Vector[(Int, Int)] = Vector()
           ): (Int, Int) = {
      instructions match {
        case Nil => position
        case h::t =>
          val newDirection = getDirection(direction, h.orientation)
          val (newPosition, newHistory) = getHistoryPosition(position, newDirection, h.value, history)
          newHistory.groupBy(identity).view.mapValues(_.size).toMap.find(_._2 > 1) match {
            case Some((p, _)) => p
            case _ => rec(newPosition, newDirection, t, newHistory)
          }
      }
    }

    getDistance(rec())
  }
}