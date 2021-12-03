package com.oodic.aoc.y2020

object Day13 extends Puzzle2020[(Int, List[String]), Int, Long] {
  override val input: (Int, List[String]) =
    getInputFile match {
      case h1 :: h2 :: _ => (h1.toInt, h2.split(',').toList)
    }

  private def chineseReminder(input: List[(Long, Long)]): Long = {
    val product: Long = input.map(_._1).product
    input.foldLeft(0L) {
      case (sum, (reminder, moduli)) =>
        val partialProduct = product / reminder
        sum +
          (partialProduct *
            math.BigInt.long2bigInt(partialProduct).modInverse(math.BigInt.long2bigInt(reminder)).toLong *
            moduli)
    } % product
  }

  override def resolveFirst(input: (Int, List[String])): Int =
    input._2
      .filter(_ != "x")
      .map(_.toInt)
      .map(bus => (bus, bus - input._1 % bus))
      .minBy(_._2) match {
      case (bus, wait) => bus * wait
    }

  override def resolveSecond(input: (Int, List[String])): Long =
    chineseReminder(input._2
      .zipWithIndex
      .filter(_._1 != "x")
      .map { case (bus, i) => (bus.toLong, bus.toLong - i)})
}
