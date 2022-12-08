package aoc.y2018

import scala.collection.MapView
import scala.util.parsing.combinator.RegexParsers

object Day04 extends Puzzle2018[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  case class Time(year: Int, month: Int, day: Int, hours: Int, minutes: Int)

  case class Interval(start: Time, end: Time)

  trait Operation{val time: Time}
  case class Shift(override val time: Time, guard: Int) extends Operation
  case class Asleep(override val time: Time) extends Operation
  case class Wakeup(override val time: Time) extends Operation

  private val timeParser = for {
    year <- "[" ~> """\d{4}""".r  ^^ (_.toInt)
    month <- "-" ~> """\d{2}""".r  ^^ (_.toInt)
    day <- "-" ~> """\d{2}""".r  ^^ (_.toInt)
    hours <- """\d{2}""".r ^^ (_.toInt)
    minutes <- ":" ~> """\d{2}""".r <~ "]" ^^ (_.toInt)
  } yield Time(year, month, day, hours, minutes)

  private val shiftParser: Parser[Shift] = for {
    time <- timeParser
    guard <- "Guard #" ~> """\d+""".r <~ "begins shift" ^^ (_.toInt)
  } yield Shift(time, guard)

  private val asleepParser: Parser[Asleep] = timeParser <~ "falls asleep" ^^ Asleep.apply

  private val wakeupParser: Parser[Wakeup] = timeParser <~ "wakes up" ^^ Wakeup.apply

  private val operationParser: Parser[Operation] = shiftParser | asleepParser | wakeupParser

  private def asleepGuards(operations: List[Operation]) = operations
    .sortBy(op => (op.time.year, op.time.month, op.time.day, op.time.hours, op.time.minutes))
    .foldLeft((Map.empty[Int, List[Operation]], 0)) {
      case ((map, _), Shift(_, guard)) => (map, guard)
      case ((map, guard), op) => (map.updated(guard, map.getOrElse(guard, List()) :+ op), guard)
    }._1
    .view.mapValues(_.grouped(2).toList.flatMap {
      case List(Asleep(start), Wakeup(end)) => (start.minutes until end.minutes).toList
    }.groupBy(identity).view.mapValues(_.size).toMap).toMap

  override def part1(input: List[String]): Int = {
    val guards = asleepGuards(input.map(parse(operationParser, _).get))
    val mostAsleepGuard = guards.view.mapValues(_.values.sum).toList.sortBy(_._2)(Ordering[Int].reverse).head._1
    val mostAsleepMinute = guards(mostAsleepGuard).toList.sortBy(_._2)(Ordering[Int].reverse).head._1
    mostAsleepGuard * mostAsleepMinute
  }

  override def part2(input: List[String]): Int = {
    val (guard, minute, _) = asleepGuards(input.map(parse(operationParser, _).get)).foldLeft((0,0,0)) {
      case ((maxGuard, maxMinute, count), guard) =>
        val (newMin, newCount) = guard._2.toList.sortBy(_._2)(Ordering[Int].reverse).head
        if (newCount > count) (guard._1, newMin, newCount)
        else (maxGuard, maxMinute, count)
    }
    guard * minute
  }
}
