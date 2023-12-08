package aoc.y2023

object Day07 extends Puzzle2023[List[(String, Int)], Int, Int] {
  override val input: List[(String, Int)] = getInputFile
    .map(_.split(" ") match
      case Array(hand, bid) => (hand, bid.toInt)
    )

  private val defaultCardOrder = "23456789TJQKA"
  private val cardsOrderWithJoker = "J23456789TQKA"

  private def replaceValuesWithJoker(values: Map[Char, Int]): Map[Char, Int] =
    if ((values.getOrElse('J', 0) == 5) || (values.getOrElse('J', 0) == 0)) values
    else {
      val valuesWithoutJoker = values.filter(_._1 != 'J')
      val joker = values('J')
      val maxValue = valuesWithoutJoker.maxBy(_._2)

      valuesWithoutJoker.updated(
        maxValue._1,
        maxValue._2 + joker
      )
    }

  private def scoreHand(hand: String, joker: Boolean = false) = {
    val valuesWithJoker = defaultCardOrder.map(c => (c, hand.count(_ == c))).filter(_._2 > 0).toMap
    val values = if (joker) replaceValuesWithJoker(valuesWithJoker) else valuesWithJoker


    if (values.exists(_._2 == 5)) 6
    else if (values.exists(_._2 == 4)) 5
    else if (values.exists(_._2 == 3) && values.exists(_._2 == 2)) 4
    else if (values.exists(_._2 == 3)) 3
    else if (values.count(_._2 == 2) > 1) 2
    else if (values.exists(_._2 == 2)) 1
    else 0
  }

  private def sortTwoHands(left: (String, Int), right: (String, Int), joker: Boolean = false): Boolean = {
    val scoreLeft = scoreHand(left._1, joker)
    val scoreRight = scoreHand(right._1, joker)

    if (scoreLeft != scoreRight) scoreLeft < scoreRight
    else {
      left._1.zip(right._1)
        .find { case (l, r) => l != r } match
        case Some((l, r)) =>
          val cards = if (joker) cardsOrderWithJoker else defaultCardOrder
          cards.indexOf(l) < cards.indexOf(r)
    }
  }

  private def sortHands(hands: List[(String, Int)], joker: Boolean = false): List[(String, Int)] =
    hands.sortWith((handLeft, handRight) => sortTwoHands(handLeft, handRight, joker))

  override def part1(input: List[(String, Int)]): Int =
    sortHands(input)
      .zipWithIndex
      .map { case ((_, bid), index) => bid * (index + 1) }
      .sum

  override def part2(input: List[(String, Int)]): Int =
    sortHands(input, joker = true)
      .zipWithIndex
      .map { case ((_, bid), index) => bid * (index + 1) }
      .sum
}
