package aoc.y2021

import scala.util.parsing.combinator.RegexParsers

object Day13 extends Puzzle2021[(List[(Int, Int)], List[String]), Int, String] with RegexParsers {
  override val input: (List[(Int, Int)], List[String]) =
    getInputFile.span(_.nonEmpty) match {
      case (cells, instructions) =>
        (
          cells.map(_.split(",") match {
            case Array(left, right) => (left.toInt, right.toInt)
          }),
          instructions.filter(_.nonEmpty)
        )
    }

  trait Fold

  case class FoldX(x: Int) extends Fold

  case class FoldY(y: Int) extends Fold

  private val parseFoldX =
    "x=" ~> """\d+""".r ^^ (value => FoldX(value.toInt))

  private val parseFoldY =
    "y=" ~> """\d+""".r ^^ (value => FoldY(value.toInt))

  private val parseInstruction: Parser[Fold] =
    "fold along" ~> (parseFoldX | parseFoldY)

  private def getDotsMap(dots: List[(Int, Int)]): Map[(Int, Int), Boolean] =
    dots.foldLeft(Map.empty[(Int, Int), Boolean])((map, pos) => map.updated(pos, true))

  private def foldUp(fold: FoldY, dots: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] =
    dots.filter {
      case ((_, y), value) =>
        value && (y >= fold.y)
    }.foldLeft(dots) {
      case (currentDots, ((x, y), _)) if y == fold.y =>
        currentDots
          .updated((x, y), false)
      case (currentDots, ((x, y), _)) =>
        currentDots
          .updated((x, y), false)
          .updated((x, (fold.y * 2) - y), true)
    }

  private def foldLeft(fold: FoldX, dots: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] =
    dots.filter {
      case ((x, _), value) =>
        value && (x >= fold.x)
    }.foldLeft(dots) {
      case (currentDots, ((x, y), _)) if x == fold.x =>
        currentDots
          .updated((x, y), false)
      case (currentDots, ((x, y), _)) =>
        currentDots
          .updated((x, y), false)
          .updated(((fold.x * 2) - x, y), true)
    }

  private def executeFolds(folds: List[Fold], dots: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] =
    folds.foldLeft(dots) {
      case (currentDots, foldX: FoldX) => foldLeft(foldX, currentDots)
      case (currentDots, foldY: FoldY) => foldUp(foldY, currentDots)
    }

  private def dotsToString(dots: Map[(Int, Int), Boolean]): String = {
    val yMax = dots.filter(_._2).map(_._1._2).max
    val xMax = dots.filter(_._2).map(_._1._1).max
    (0 to yMax).map(y =>
      (0 to xMax).map(x =>
        dots.get((x, y)) match {
          case Some(true) => "#"
          case _ => "."
        }
      ).mkString
    ).mkString("\n")
  }

  override def part1(input: (List[(Int, Int)], List[String])): Int =
    input match {
      case (dots, instructions) =>
        executeFolds(
          List(instructions.map(parse(parseInstruction, _).get).head),
          getDotsMap(dots)
        ).count(_._2)
    }

  override def part2(input: (List[(Int, Int)], List[String])): String =
    input match {
      case (dots, instructions) =>
        dotsToString(executeFolds(
          instructions.map(parse(parseInstruction, _).get),
          getDotsMap(dots)
        ))
    }
}
