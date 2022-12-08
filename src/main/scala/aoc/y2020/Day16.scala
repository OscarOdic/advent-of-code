package aoc.y2020

import scala.util.parsing.combinator.RegexParsers

object Day16 extends Puzzle2020[(List[String], List[String], List[String]), Int, Long] with RegexParsers {
  override val input: (List[String], List[String], List[String]) =
    getInputFile.foldLeft(List(List.empty[String]))((lines, line) =>
      if (line == "")
        List.empty[String] +: lines
      else
        lines match {
          case h :: t => (h :+ line) :: t
        }
    ).reverse match {
      case List(x, y, z) => (x, y, z)
    }

  case class Note(rules: List[Rule], yourTicket: List[Int], nearbyTickets: List[List[Int]])

  case class Rule(key: String, value: List[(Int, Int)])

  private val ruleParser = for {
    key <- """[\w\s]+""".r <~ ":"
    value <- rep1sep(for {
      min <- """\d+""".r <~ "-" ^^ (_.toInt)
      max <- """\d+""".r ^^ (_.toInt)
    } yield (min, max), "or")
  } yield Rule(key, value)

  private def parseNote(note: (List[String], List[String], List[String])): Note =
    Note(
      note._1.map(parse(ruleParser, _).get),
      note._2.tail.head.split(',').map(_.toInt).toList,
      note._3.tail.map(_.split(',').map(_.toInt).toList)
    )

  private def invalidValues(rules: List[Rule], ticket: List[Int]): List[Int] =
    ticket
      .filter(value => rules.forall(_.value.forall {
        case (min, max) => value < min || value > max
      }))

  private def isValid(rules: List[Rule], ticket: List[Int]): Boolean =
    ticket
      .forall(value => rules.exists(_.value.exists {
        case (min, max) => value >= min && value <= max
      }))

  override def part1(input: (List[String], List[String], List[String])): Int =
    parseNote(input) match {
      case Note(rules, yourTicket, nearbyTickets) =>
        (yourTicket +: nearbyTickets).flatMap(ticket => invalidValues(rules, ticket))
          .sum
    }

  private def potentialRulePositions(rule: Rule, tickets: List[List[Int]]): List[Int] =
    tickets
      .transpose
      .zipWithIndex
      .filter(
        _._1.forall(value => rule.value.exists {
          case (min, max) => value >= min && value <= max
        })
      ).map(_._2)

  private def getRulesWithPositions(rules: List[Rule], potentialPositions: List[List[Int]], map: Map[Rule, Int] = Map.empty): Map[Rule, Int] =
    rules
      .zipWithIndex
      .foldLeft((Map.empty[Rule, Int], potentialPositions)) {
        case ((currentMap, currentPotentialPositions), (rule, index)) =>
          currentPotentialPositions.zipWithIndex.find(_._1.size == 1) match {
            case None => (currentMap, currentPotentialPositions)
            case Some((singleElement, position)) =>
              (currentMap.updated(rules(position), singleElement.head),
                currentPotentialPositions.map(_.filter(_ != singleElement.head)))
          }
      }._1

  override def part2(input: (List[String], List[String], List[String])): Long =
    parseNote(input) match {
      case Note(rules, yourTicket, nearbyTickets) =>
        val validTickets = (yourTicket +: nearbyTickets).filter(isValid(rules, _))
        val possibleRulePositions = rules.map(potentialRulePositions(_, validTickets))
        getRulesWithPositions(rules, possibleRulePositions)
          .filter(_._1.key.startsWith("departure"))
          .values
          .map(yourTicket(_).toLong)
          .product
    }
}
