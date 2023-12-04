package aoc.y2023

import scala.util.parsing.combinator.RegexParsers

object Day04 extends Puzzle2023[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  private case class ScratchCard(id: Int, winingNumbers: List[Int], playerNumbers: List[Int]) {
    private def getNumberWins: Int =
      playerNumbers
        .intersect(winingNumbers)
        .length

    def getPoints: Int =
      getNumberWins match
        case 0 => 0
        case n => math.pow(2, n - 1).toInt

    def getCopies: List[Int] =
      getNumberWins match
        case 0 => List.empty
        case n => (id + 1 until id + n + 1).toList
  }

  private def scratchCardParser: Parser[ScratchCard] =
    for {
      id <- "Card" ~> """\d+""".r <~ ":" ^^ (_.toInt)
      winningNumbers <- rep("""\d+""".r ^^ (_.toInt)) <~ "|"
      playerNumbers <- rep("""\d+""".r ^^ (_.toInt))
    } yield ScratchCard(id, winningNumbers, playerNumbers)

  private def processRules(scratchCards: List[ScratchCard]): Int =
    scratchCards
      .foldLeft(scratchCards.map(_.id -> 1).toMap)((numberCards, card) =>
        card
          .getCopies
          .foldLeft(numberCards)((accNumberCards, copy) =>
            accNumberCards.updated(copy, accNumberCards(copy) + accNumberCards(card.id))
          )
      ).values.sum

  override def part1(input: List[String]): Int = {
    input
      .map(parse(scratchCardParser, _).get)
      .map(_.getPoints)
      .sum
  }

  override def part2(input: List[String]): Int = {
    processRules(
      input
        .map(parse(scratchCardParser, _).get)
    )
  }
}
