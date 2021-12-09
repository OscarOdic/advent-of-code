package com.oodic.aoc.y2016

import java.security.MessageDigest

object Day17 extends Puzzle2016[String, String, Int] {
  override val input: String = "rrrbmfta"

  case class State(x: Int, y: Int, path: String)

  private def hash(value: String) =
    MessageDigest.getInstance("MD5").digest(value.getBytes("ASCII")).take(2).map("%02x".format(_)).mkString

  private def isOpen(value: Char) = value match {
    case 'b' | 'c' | 'd' | 'e' | 'f' => true
    case _ => false
  }

  private def possibility(value: Char, direction: String, state: State) = direction match {
    case "up" if state.y < 3 && isOpen(value) => Some(state.copy(y = state.y + 1, path = state.path + "U"))
    case "down" if state.y > 0 && isOpen(value) => Some(state.copy(y = state.y - 1, path = state.path + "D"))
    case "left" if state.x > 0 && isOpen(value) => Some(state.copy(x = state.x - 1, path = state.path + "L"))
    case "right" if state.x < 3 && isOpen(value) => Some(state.copy(x = state.x + 1, path = state.path + "R"))
    case _ => None
  }

  private def correctPossibility(state: State) = state.x == 3 && state.y == 0

  private def nextPossibilities(possibilities: Vector[State], passcode: String): Vector[State] =
    possibilities.flatMap(s =>
      hash(passcode + s.path).toList.zip(List("up", "down", "left", "right")) flatMap {
        case (value, direction) =>
          possibility(value, direction, s)
      }
    )

  private def shortestPath(passcode: String) = {
    def rec(possibilities: Vector[State] = Vector(State(0, 3, ""))): String = possibilities.find(correctPossibility) match {
      case Some(State(_, _, path)) => path
      case _ => rec(nextPossibilities(possibilities, passcode))
    }
    rec()
  }

  private def longestPath(passcode: String) = {
    def rec(possibilities: Vector[State] = Vector(State(0, 3, "")), steps: Int = 0): Int = {
      val (finished, unfinished) = possibilities.partition(correctPossibility)
      val finishedSteps = if (finished.nonEmpty) steps else 0
      val unfinishedSteps = if (unfinished.isEmpty) 0 else rec(nextPossibilities(unfinished, passcode), steps + 1)
      finishedSteps max unfinishedSteps
    }
    rec()
  }

  override def part1(input: String): String = shortestPath(input)

  override def part2(input: String): Int = longestPath(input)
}