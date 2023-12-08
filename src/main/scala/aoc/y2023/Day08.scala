package aoc.y2023

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day08 extends Puzzle2023[(String, List[String]), Long, Long] with RegexParsers {
  override val input: (String, List[String]) = getInputFile match
    case head :: tail => (head, tail.tail)

  type Nodes = Map[String, (String, String)]

  def lcm(list: Seq[Long]): Long = list.foldLeft(1L) {
    (a, b) => b * a /
      LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  private def nodeParser: Parser[(String, (String, String))] =
    for {
      key <- """\w{3}""".r <~ "="
      left <- "(" ~> """\w{3}""".r <~ ","
      right <- """\w{3}""".r <~ ")"
    } yield (key, (left, right))

  private def parseNodes(input: List[String]): Nodes =
    input
      .map(parse(nodeParser, _).get)
      .toMap

  @tailrec
  private def stepsTo(
                       instructions: String,
                       nodes: Nodes,
                       stop: String => Boolean = _ == "ZZZ",
                       value: String = "AAA",
                       step: Long = 0
                     ): Long =
    if (stop(value)) step
    else instructions.charAt((step % instructions.length).toInt) match
      case 'L' => stepsTo(instructions, nodes, stop, nodes(value)._1, step + 1)
      case 'R' => stepsTo(instructions, nodes, stop, nodes(value)._2, step + 1)

  private def ghostStepsTo(instructions: String, nodes: Nodes, values: List[String]): Long =
    lcm(values.map(stepsTo(instructions, nodes, _.endsWith("Z"), _)))

  override def part1(input: (String, List[String])): Long =
    stepsTo(input._1, parseNodes(input._2))

  override def part2(input: (String, List[String])): Long = {
    val nodes = parseNodes(input._2)
    val ghostPositions = nodes.toList.filter(_._1.endsWith("A")).map(_._1)
    ghostStepsTo(input._1, nodes, ghostPositions)
  }
}
