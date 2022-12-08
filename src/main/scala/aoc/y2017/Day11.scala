package aoc.y2017

object Day11 extends Puzzle2017[List[String], Int, Int] {
  override val input: List[String] = getInputFile.mkString.split(",").toList

  def move(position: (Double, Double), direction: String): (Double, Double) = direction match {
    case "n" => (position._1, position._2 + 1)
    case "s" => (position._1, position._2 - 1)
    case "ne" => (position._1 + 1, position._2 + 0.5)
    case "se" => (position._1 + 1, position._2 - 0.5)
    case "nw" => (position._1 - 1, position._2 + 0.5)
    case "sw" => (position._1 - 1, position._2 - 0.5)
  }

  def distance(position: (Double, Double)): Int = {
    if (position == (0.0,0.0)) 0
    else {
      val newPosition =
        if (position._1 > 0 && position._2 > 0) move(position, "sw")
        else if (position._1 > 0 && position._2 < 0) move(position, "nw")
        else if (position._1 < 0 && position._2 > 0) move(position, "se")
        else if (position._1 < 0 && position._2 < 0) move(position, "ne")
        else if (position._2 > 0) move(position, "s")
        else move(position, "n")
      distance(newPosition) + 1
    }
  }

  override def part1(input: List[String]): Int = {
    val finalPosition = input.foldLeft((0.0,0.0))((position, direction) => move(position, direction))
    distance(finalPosition)
  }

  override def part2(input: List[String]): Int =
    input.foldLeft(((0.0,0.0), 0)) {
      case ((position, maxDistance), direction) =>
        val newPosition = move(position, direction)
        (newPosition, math.max(distance(newPosition), maxDistance))
    }._2
}
