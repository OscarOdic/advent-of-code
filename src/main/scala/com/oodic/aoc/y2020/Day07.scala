package com.oodic.aoc.y2020

import scala.util.parsing.combinator.RegexParsers

object Day07 extends Puzzle2020[List[String], Int, Int] with RegexParsers {
  override val input: List[String] =
    getInputFile

  case class Bag(name: String, contains: Seq[(Int, String)])

  private val noOtherBagsParser = for {
    _ <- "no other bags".r
  } yield Seq.empty[(Int, String)]

  private val nonEmptyBagParser = for {
    n <- """\d+""".r ^^ (_.toInt)
    name <- """\w+\s\w+""".r <~ """bags?""".r
  } yield (n, name)

  private val containParser = rep1sep(nonEmptyBagParser, ",") | noOtherBagsParser

  private val bagParser = for {
    name <- """\w+\s\w+""".r <~ """bags?""".r
    contain <- "contain" ~> containParser <~ "."
  } yield Bag(name, contain)

  private def containBag(bags: List[Bag], bag: String): List[String] = {
    val bagsContains = bags
      .filter(_.contains.map(_._2).contains(bag))
      .map(_.name)

    if (bagsContains.isEmpty)
      List()
    else
      (bagsContains ++ bagsContains.flatMap(containBag(bags, _)))
        .distinct
  }

  private def numbersBag(bags: List[Bag], bag: String): Int =
    bags.find(_.name == bag) match {
      case Some(Bag(_, contains)) if contains.nonEmpty =>
        contains.foldLeft(0) {
          case (total, (n, bag)) => total + n + n * numbersBag(bags, bag)
        }
      case _ => 0
    }


  override def resolveFirst(input: List[String]): Int =
    containBag(input
      .map(parse(bagParser, _).get), "shiny gold")
      .size

  override def resolveSecond(input: List[String]): Int =
    numbersBag(input
      .map(parse(bagParser, _).get), "shiny gold")
}
