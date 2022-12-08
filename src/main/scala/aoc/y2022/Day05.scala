package aoc.y2022

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Day05 extends Puzzle2022[(List[String], List[String]), String, String] with RegexParsers {
  private def splitInput(input: List[String]): (List[String], List[String]) = {
    val index = input.indexOf("")
    input.splitAt(index) match
      case (header, operations) => (header, operations.tail)
  }

  override val input: (List[String], List[String]) =
    splitInput(getInputFile)

  private case class Operation(quantity: Int, from: Int, to: Int) {
    def execute(stacks: Map[Int, List[Char]], reverse: Boolean): Map[Int, List[Char]] = {
      val values = stacks(from).take(quantity)
      stacks
        .updated(
          from,
          stacks(from).drop(quantity)
        )
        .updated(
          to,
          (if (reverse) values.reverse else values) ++ stacks.getOrElse(to, List.empty[Char])
        )
    }
  }

  private def operationParser: Parser[Operation] =
    for {
      quantity <- "move" ~> """\d+""".r ^^ (_.toInt)
      from <- "from" ~> """\d""".r ^^ (_.toInt)
      to <- "to" ~> """\d""".r ^^ (_.toInt)
    } yield Operation(quantity, from, to)

  private def executeOperations(
                                 stacks: Map[Int, List[Char]],
                                 operations: List[String],
                                 reverse: Boolean
                               ): Map[Int, List[Char]] =
    operations.foldLeft(stacks)((stacksAcc, op) =>
      parse(operationParser, op).get
        .execute(stacksAcc, reverse)
    )

  private def parseValue(value: String): Option[Char] =
    if (value.startsWith("["))
      Some(value.charAt(1))
    else
      None

  private def parseLine(line: String): List[Option[Char]] =
    if (line.length < 3)
      List.empty
    else
      parseValue(line.take(3)) +: parseLine(line.drop(4))

  private def parseStacks(header: List[String]): Map[Int, List[Char]] = {
    val reversedHeader = header.reverse.tail

    reversedHeader
      .map(parseLine)
      .foldLeft(Map.empty[Int, List[Char]]) ((map, line) =>
        line.zipWithIndex.foldLeft(map) {
          case (accMap, (Some(value), index)) =>
            accMap.updated(
              index + 1,
              value +: accMap.getOrElse(index + 1, List.empty[Char])
            )
          case (accMap, _) => accMap
        }
      )
  }

  private def resolve(input: (List[String], List[String]), reverse: Boolean): String = {
    val stacks = parseStacks(input._1)
    executeOperations(stacks, input._2, reverse)
      .toList
      .sorted
      .map(_._2.head)
      .mkString
  }

  override def part1(input: (List[String], List[String])): String =
    resolve(input, true)

  override def part2(input: (List[String], List[String])): String =
    resolve(input, false)
}
