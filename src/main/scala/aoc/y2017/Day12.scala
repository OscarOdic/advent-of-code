package aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day12 extends Puzzle2017[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  val communicationParser: Parser[Map[Int, Set[Int]]] = for {
    program <- """\d+""".r ^^ (_.toInt)
    _ <- """<->""".r
    programs <- rep1sep("""\d+""".r ^^ (_.toInt), ", ")
  } yield Map(program -> programs.toSet)

  def getGroup(origin: Int, communications: Map[Int, Set[Int]]): Set[Int] = {
    def rec(contains: Set[Int] = Set(origin), communicate: Set[Int] = Set(origin)): Set[Int] = {
      val next = communicate.flatMap(communications).diff(contains)
      if (next.isEmpty) contains
      else rec(contains.union(next), next)
    }
    rec()
  }

  override def part1(input: List[String]): Int = {
    val communications = input.map(parse(communicationParser, _).get).reduce(_ ++ _)
    getGroup(0, communications).size
  }

  override def part2(input: List[String]): Int = {
    val communications = input.map(parse(communicationParser, _).get).reduce(_ ++ _)
    def rec(keys: List[Int] = communications.keys.toList, n: Int = 0): Int = keys match {
      case Nil => n
      case h::_ => rec(keys.diff(getGroup(h, communications).toList), n+1)
    }
    rec()
  }
}
