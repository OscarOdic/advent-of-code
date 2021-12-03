package com.oodic.aoc.y2020

import scala.util.parsing.combinator.RegexParsers

object Day12 extends Enumeration with Puzzle2020[List[String], Int, Int] with RegexParsers {
  override val input: List[String] =
    getInputFile

  type Direction = Value
  val East, North, West, South = Value

  case class Ship(x: Int = 0, y: Int = 0, direction: Direction = East, waypointX: Int = 10, waypointY: Int = 1)

  case class Instruction(action: Char, value: Int) {
    def executeFirstRule(ship: Ship): Ship =
      action match {
        case 'N' => ship.copy(y = ship.y + value)
        case 'E' => ship.copy(x = ship.x + value)
        case 'W' => ship.copy(x = ship.x - value)
        case 'S' => ship.copy(y = ship.y - value)
        case 'L' => ship.direction match {
          case East => value match {
            case 90 => ship.copy(direction = North)
            case 180 => ship.copy(direction = West)
            case 270 => ship.copy(direction = South)
          }
          case South => value match {
            case 90 => ship.copy(direction = East)
            case 180 => ship.copy(direction = North)
            case 270 => ship.copy(direction = West)
          }
          case West => value match {
            case 90 => ship.copy(direction = South)
            case 180 => ship.copy(direction = East)
            case 270 => ship.copy(direction = North)
          }
          case North => value match {
            case 90 => ship.copy(direction = West)
            case 180 => ship.copy(direction = South)
            case 270 => ship.copy(direction = East)
          }
        }
        case 'R' => ship.direction match {
          case East => value match {
            case 90 => ship.copy(direction = South)
            case 180 => ship.copy(direction = West)
            case 270 => ship.copy(direction = North)
          }
          case South => value match {
            case 90 => ship.copy(direction = West)
            case 180 => ship.copy(direction = North)
            case 270 => ship.copy(direction = East)
          }
          case West => value match {
            case 90 => ship.copy(direction = North)
            case 180 => ship.copy(direction = East)
            case 270 => ship.copy(direction = South)
          }
          case North => value match {
            case 90 => ship.copy(direction = East)
            case 180 => ship.copy(direction = South)
            case 270 => ship.copy(direction = West)
          }
        }
        case 'F' => ship.direction match {
          case East => ship.copy(x = ship.x + value)
          case South => ship.copy(y = ship.y - value)
          case West => ship.copy(x = ship.x - value)
          case North => ship.copy(y = ship.y + value)
        }
      }

    def executeSecondRule(ship: Ship): Ship =
      action match {
        case 'N' => ship.copy(waypointY = ship.waypointY + value)
        case 'E' => ship.copy(waypointX = ship.waypointX + value)
        case 'W' => ship.copy(waypointX = ship.waypointX - value)
        case 'S' => ship.copy(waypointY = ship.waypointY - value)
        case 'L' => value match {
          case 90 => ship.copy(waypointX = -ship.waypointY, waypointY = ship.waypointX)
          case 180 => ship.copy(waypointX = -ship.waypointX, waypointY = -ship.waypointY)
          case 270 => ship.copy(waypointX = ship.waypointY, waypointY = -ship.waypointX)
        }
        case 'R' => value match {
          case 90 => ship.copy(waypointX = ship.waypointY, waypointY = -ship.waypointX)
          case 180 => ship.copy(waypointX = -ship.waypointX, waypointY = -ship.waypointY)
          case 270 => ship.copy(waypointX = -ship.waypointY, waypointY = ship.waypointX)
        }
        case 'F' => ship.copy(x = ship.x + (ship.waypointX * value), y = ship.y + (ship.waypointY * value))
      }
  }

  private val instructionParser = for {
    action <- """\w""".r ^^ (_.head)
    value <- """\d+""".r ^^ (_.toInt)
  } yield Instruction(action, value)

  override def resolveFirst(input: List[String]): Int =
    input
      .map(parse(instructionParser, _).get)
      .foldLeft(Ship())((ship, instruction) => instruction.executeFirstRule(ship)) match {
      case Ship(x, y, _, _, _) => math.abs(x) + math.abs(y)
    }

  override def resolveSecond(input: List[String]): Int =
    input
      .map(parse(instructionParser, _).get)
      .foldLeft(Ship())((ship, instruction) => instruction.executeSecondRule(ship)) match {
      case Ship(x, y, _, _, _) => math.abs(x) + math.abs(y)
    }
}
