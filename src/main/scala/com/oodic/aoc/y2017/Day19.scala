package com.oodic.aoc.y2017

object Day19 extends Puzzle2017[Map[(Int, Int),String], String, Int]  {
  override val input: Map[(Int, Int), String] = toMap(getInputFile)

  trait Direction
  object Up extends Direction
  object Down extends Direction
  object Right extends Direction
  object Left extends Direction

  def toMap(input: List[String]): Map[(Int, Int), String] = {
    val list = input.map(_.toList.map(_.toString))
    val coords = for {
      y <- list.indices
      x <- list(y).indices
    } yield ((x,y), list(y)(x))
    coords.filter(_._2 != " ").toMap
  }

  private def next(pos: (Int, Int), direction: Direction) = direction match {
    case Up => (pos._1, pos._2 - 1)
    case Down => (pos._1, pos._2 + 1)
    case Left => (pos._1 - 1, pos._2)
    case Right => (pos._1 + 1, pos._2)
  }

  private def resolve(input: Map[(Int, Int), String]): (String, Int) = {
    def rec(pos: (Int, Int), value: String = "", direction: Direction = Down, nbStep: Int = 0): (String, Int) = input.get(pos) match {
      case None => (value, nbStep)
      case Some("+") =>
        if (input.get(pos._1, pos._2 + 1).isDefined && direction != Up) rec((pos._1, pos._2 + 1), value, Down, nbStep + 1)
        else if (input.get(pos._1, pos._2 - 1).isDefined && direction != Down) rec((pos._1, pos._2 - 1), value, Up, nbStep + 1)
        else if (input.get(pos._1 - 1, pos._2).isDefined && direction != Right) rec((pos._1 - 1, pos._2), value, Left, nbStep + 1)
        else rec((pos._1 + 1, pos._2), value, Right, nbStep + 1)
      case Some(char) if """\w+""".r.pattern.matcher(char).matches =>
        rec(next(pos, direction), value ++ char, direction, nbStep + 1)
      case d =>
        rec(next(pos, direction), value, direction, nbStep + 1)
    }
    rec(input.filter { case ((_, y), value) => y == 0}.head._1)
  }

  override def resolveFirst(input: Map[(Int, Int), String]): String = resolve(input)._1

  override def resolveSecond(input: Map[(Int, Int), String]): Int = resolve(input)._2
}
