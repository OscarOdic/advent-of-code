package com.oodic.aoc.y2018

import scala.util.parsing.combinator.RegexParsers

object Day03 extends Puzzle2018[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  case class Claim(id: Int, leftEdge: Int, topEdge: Int, width: Int, height: Int)

  private val parseClaim: Parser[Claim] = for {
    id <- "#" ~> """\d+""".r ^^ (_.toInt)
    leftEdge <- "@" ~> """\d+""".r ^^ (_.toInt)
    topEdge <- "," ~> """\d+""".r ^^ (_.toInt)
    width <- ":" ~> """\d+""".r ^^ (_.toInt)
    height <- "x" ~> """\d+""".r ^^ (_.toInt)
  } yield Claim(id, leftEdge, topEdge, width, height)

  private def getCoords(claim: Claim): Seq[(Int, Int)] = for {
    x <- claim.leftEdge until claim.leftEdge + claim.width
    y <- claim.topEdge until claim.topEdge + claim.height
  } yield (x,y)

  private def coordsClaims(claims: Seq[Claim]): Map[(Int, Int), List[Int]] = claims
    .foldLeft(Map.empty[(Int, Int), List[Int]])((map, claim) =>
      getCoords(claim).foldLeft(map)((currentMap, coord) =>
        currentMap.updated(coord, claim.id +: currentMap.getOrElse(coord, List()))
      )
    )

  override def resolveFirst(input: List[String]): Int =
    coordsClaims(input.map(parse(parseClaim, _).get)).count(_._2.size > 1)

  override def resolveSecond(input: List[String]): Int = {
    val claims = input.map(parse(parseClaim, _).get)
    val coords = coordsClaims(claims)
    claims.map(_.id).filterNot(id => coords.exists(coord => coord._2.contains(id) && coord._2.size > 1)).head
  }
}
