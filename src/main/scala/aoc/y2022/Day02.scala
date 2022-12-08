package aoc.y2022

object Day02 extends Puzzle2022[List[(String, String)], Int, Int] {
  override val input: List[(String, String)] = getInputFile
    .map(_.split(' ') match
      case Array(valueLeft, valueRight) => (valueLeft, valueRight)
    )

  private def scoreShape(shape: String): Int = shape match
    case "X" => 1
    case "Y" => 2
    case "Z" => 3

  private def scoreRound(opponent: String, player: String): Int =
    ((opponent, player) match
      case ("A", "Z") | ("B", "X") | ("C", "Y") => 0
      case ("A", "X") | ("B", "Y") | ("C", "Z") => 3
      case ("A", "Y") | ("B", "Z") | ("C", "X") => 6
    ) + scoreShape(player)

  private def scoreRoundPart2(opponent: String, result: String): Int =
    opponent match
      case "A" => result match
        case "X" => 3
        case "Y" => 4
        case "Z" => 8
      case "B" => result match
        case "X" => 1
        case "Y" => 5
        case "Z" => 9
      case "C" => result match
        case "X" => 2
        case "Y" => 6
        case "Z" => 7

  override def part1(input: List[(String, String)]): Int =
    input
      .map {
        case (opponent, player) => scoreRound(opponent, player)
      }.sum

  override def part2(input: List[(String, String)]): Int =
    input
      .map {
        case (opponent, player) => scoreRoundPart2(opponent, player)
      }.sum
}
