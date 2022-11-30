package com.oodic.aoc.y2016

import scala.util.parsing.combinator.RegexParsers

object Day10 extends Puzzle2016[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  trait Type
  object Output extends Type
  object Bot extends Type

  trait Instruction
  case class Init(value: Int) extends Instruction
  case class Give(low: (Type, Int), high: (Type, Int)) extends Instruction

  private def parseInit: Parser[(Int, Init)] = for {
    value <- "value" ~> """\d+""".r ^^ (_.toInt)
    bot <- "goes to bot" ~> """\d+""".r ^^ (_.toInt)
  } yield bot -> Init(value)

  private def parseType: Parser[Type] = """bot|output""".r ^^ {
    case "bot" => Bot
    case "output" => Output
  }

  private def parseGive: Parser[(Int, Give)] = for {
    bot <- "bot" ~> """\d+""".r ^^ (_.toInt)
    typeLow <- "gives low to" ~> parseType
    botLow <- """\d+""".r ^^ (_.toInt)
    typeHigh <- "and high to" ~> parseType
    botHigh <- """\d+""".r ^^ (_.toInt)
  } yield bot -> Give((typeLow, botLow), (typeHigh, botHigh))

  private def parseInstruction: Parser[(Int, Instruction)] = parseInit | parseGive

  private def initBots(input: List[(Int, Instruction)]) = {
    val (init, give) = input.partition(_._2.isInstanceOf[Init])

    (init.asInstanceOf[List[(Int, Init)]].map(init =>
      init._1 -> init._2.value
    ).groupBy(_._1).view.mapValues(_.map(_._2).toVector).toMap, give.asInstanceOf[List[(Int, Give)]].toMap)
  }

  private def execGive(bot: Int, values: Vector[Int], give: Give, bots: Map[Int, Vector[Int]], outputs: Map[Int, Int]) = {
    val (botOp, outputOp) = List((give.low, values(0)), (give.high, values(1))).partition(_._1._1 == Bot)

    (botOp.foldLeft(bots.updated(bot, Vector.empty))((botsAcc, give) => botsAcc.updated(give._1._2, (botsAcc.getOrElse(give._1._2, Vector.empty):+ give._2).sorted)),
      outputOp.foldLeft(outputs)((outputsAcc, give) => outputsAcc.updated(give._1._2, give._2)))
  }

  private def getResponsible(low: Int, high: Int, bots: Map[Int, Vector[Int]], gives: Map[Int, Give]): Int =
    bots.find(_._2.length > 1) match {
      case Some((bot, Vector(l, h))) if l == low && h == high => bot
      case Some((bot, values)) => gives.find(_._1 == bot) match {
        case Some((_, g)) => getResponsible(low, high, execGive(bot, values, g, bots, Map())._1, gives)
        case _ => -1
      }
      case _ => -1
    }

  private def executeAll(bots: Map[Int, Vector[Int]], outputs: Map[Int, Int], gives: Map[Int, Give]): (Map[Int, Vector[Int]], Map[Int, Int]) = {
    bots.find(_._2.length > 1) match {
      case Some((bot, values)) => gives.find(_._1 == bot) match {
        case Some((_, g)) =>
          val (newBots, newOutputs) = execGive(bot, values, g, bots, outputs)
          executeAll(newBots, newOutputs, gives)
        case _ => (bots, outputs)
      }
      case _ => (bots, outputs)
    }
  }

  override def part1(input: List[String]): Int = {
    val (init, gives) = initBots(input.map(parse(parseInstruction, _).get))
    getResponsible(17, 61, init, gives)
  }

  override def part2(input: List[String]): Int = {
    val (init, gives) = initBots(input.map(parse(parseInstruction, _).get))
    val outputs = executeAll(init, Map(), gives)._2
    outputs(0) * outputs(1) * outputs(2)
  }
}