package com.oodic.aoc

import scala.util.parsing.combinator.RegexParsers

object Day21 extends PuzzleDay[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  type Pattern = Vector[Vector[Boolean]]

  val defaultPattern: Pattern = Vector(
    Vector(false, true, false),
    Vector(false, false, true),
    Vector(true, true, true)
  )

  private def allReverses(pattern: Pattern) = List(
    pattern,
    pattern.reverse,
    pattern.map(_.reverse),
    pattern.map(_.reverse).reverse
  )

  private def allPositions(pattern: Pattern) =
    allReverses(pattern) ++ allReverses(pattern.transpose)

  case class Rule(in: Pattern, out: Pattern)

  private def toBooleanVector(value: String) =
    value.toVector.map {
      case '#' => true
      case _ => false
    }

  val ruleParser: Parser[Rule] = for {
    in <- rep1sep("""(\.|#)+""".r ^^ toBooleanVector, "/")
    out <- "=>" ~> rep1sep("""(\.|#)+""".r ^^ toBooleanVector, "/")
  } yield Rule(in.toVector, out.toVector)

  private def equals(value1: Pattern, value2: Pattern): Boolean =
    allPositions(value2).contains(value1)

  private def transformBlock(rules: List[Rule])(list: Pattern) = {
    rules.find(rule => {
      if (equals(list, rule.in)) {
        true
      } else false
    }).map(_.out).get
  }

  private def transformPattern(pattern: Pattern, rules2: List[Rule], rules3: List[Rule]): Pattern = {
    val size = if (pattern.size % 2 == 0) 2 else 3
    val rules = if (size == 2) rules2 else rules3

    (0 until pattern.size / size).map(i =>
      (0 until pattern.size / size).map(j =>
        transformBlock(rules)(pattern.slice(i*size, i*size+size).map(_.slice(j*size, j*size+size)))
      ).reduce((v1, v2) => v1.zip(v2).map { case (left, right) => left ++ right })
    ).reduce(_ ++ _)
  }

  def resolve(input: List[String], nbSteps: Int): Int = {
    val rules = input.map(parse(ruleParser, _).get).partition(_.in.size == 2)
    (0 until nbSteps).foldLeft(defaultPattern)((pattern, _) => transformPattern(pattern, rules._1, rules._2)).map(_.count(b => b)).sum
  }

  override def resolveFirst(input: List[String]): Int =
    resolve(input, 5)

  override def resolveSecond(input: List[String]): Int =
    resolve(input, 18)
}
