package aoc.y2021

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day14 extends Puzzle2021[(String, List[String]), Long, Long] with RegexParsers {
  override val input: (String, List[String]) =
    getInputFile match {
      case h :: _ :: t =>
        (h, t)
    }

  private val parseRule = for {
    pairLeft <- """\w""".r ^^ (_.head)
    pairRight <- """\w""".r ^^ (_.head)
    element <- "->" ~> """\w""".r ^^ (_.head)
  } yield (pairLeft, pairRight) -> element

  private def polymerToMap(polymer: String): Map[(Char, Char), Long] =
    polymer.toList.sliding(2).foldLeft(Map.empty[(Char, Char), Long]) {
      case (currentMap, List(left, right)) => currentMap.get((left, right)) match {
        case Some(value) =>
          currentMap.updated((left, right), value + 1)
        case _ =>
          currentMap.updated((left, right), 1)
      }
    }

  private def step(polymer: Map[(Char, Char), Long], rules: Map[(Char, Char), Char]): Map[(Char, Char), Long] =
    polymer.foldLeft(Map.empty[(Char, Char), Long]) {
      case (currentMap, (pair, n)) =>
        rules.find(_._1 == pair) match {
          case Some(((left, right), element)) =>
            currentMap
              .updated((left, element), currentMap.getOrElse((left, element), 0L) + n)
              .updated((element, right), currentMap.getOrElse((element, right), 0L) + n)
          case _ =>
            currentMap.updated(pair, n)
        }
    }

  @tailrec
  private def process(polymer: Map[(Char, Char), Long], rules: Map[(Char, Char), Char], steps: Int = 10): Map[(Char, Char), Long] =
    if (steps == 0) polymer
    else process(step(polymer, rules), rules, steps - 1)

  private def polymerMapToGroups(firstLetter: Char, polymer: Map[(Char, Char), Long]): Map[Char, Long] = {
    val mapRight = polymer
      .toList
      .map {
        case ((_, right), count) => right -> count
      }.groupBy(_._1)
      .map {
        case (char, list) => char -> list.map(_._2).sum
      }
    mapRight.updated(firstLetter, mapRight.getOrElse(firstLetter, 0))
  }

  override def part1(input: (String, List[String])): Long = {
    val pairs = process(polymerToMap(input._1), input._2.map(parse(parseRule, _).get).toMap)
    val groups = polymerMapToGroups(input._1.head, pairs).values

    groups.max - groups.min
  }

  override def part2(input: (String, List[String])): Long = {
    val pairs = process(polymerToMap(input._1), input._2.map(parse(parseRule, _).get).toMap, 40)
    val groups = polymerMapToGroups(input._1.head, pairs).values

    groups.max - groups.min
  }
}
