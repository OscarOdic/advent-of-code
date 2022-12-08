package aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day24 extends Puzzle2017[Vector[String], Int, Int] with RegexParsers {
  override val input: Vector[String] = getInputFile.toVector

  type Bridge = Vector[(Int, Int)]

  val commandParser: Parser[(Int, Int)] = for {
    left <- """\d+""".r ^^ (_.toInt)
    _ <- "/".r
    right <- """\d+""".r ^^ (_.toInt)
  } yield (left, right)

  private def getStrength(bridge: Bridge) = bridge.map(c => c._1 + c._2).sum

  private def getAllBridges(commands: Vector[(Int, Int)], start: Int = 0): Vector[Bridge] = {
    val left = commands.filter(_._1 == start)
    val right = commands.filter(_._2 == start).diff(left)

    val leftBridges = left.flatMap(c =>
      getAllBridges(commands.filterNot(_ == c), c._2).map(b => c +: b)
    )
    val rightBridges = right.flatMap(c =>
      getAllBridges(commands.filterNot(_ == c), c._1).map(b => c +: b)
    )

    left +: right +: (leftBridges ++ rightBridges)
  }

  override def part1(input: Vector[String]): Int =
    getAllBridges(input.map(parse(commandParser, _).get)).map(getStrength).max

  override def part2(input: Vector[String]): Int = {
    val bridges = getAllBridges(input.map(parse(commandParser, _).get))
    val maxLong = bridges.map(_.size).max
    bridges.filter(_.size == maxLong).map(getStrength).max
  }
}
