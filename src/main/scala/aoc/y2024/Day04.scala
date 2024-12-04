package aoc.y2024

object Day04 extends Puzzle2024[List[List[Char]], Int, Int] {
  override val input: List[List[Char]] = getInputFile.map(_.toList)

  enum Direction:
    case Left, Right, Up, Down, UpLeft, UpRight, DownLeft, DownRight

  private def isXmax(x: Int, y: Int, input: List[List[Char]], direction: Direction): Boolean =
    val nextCoord = direction match
      case Direction.Left => List((x - 1, y), (x - 2, y), (x - 3, y))
      case Direction.Right => List((x + 1, y), (x + 2, y), (x + 3, y))
      case Direction.Up => List((x, y - 1), (x, y - 2), (x, y - 3))
      case Direction.Down => List((x, y + 1), (x, y + 2), (x, y + 3))
      case Direction.UpLeft => List((x - 1, y - 1), (x - 2, y - 2), (x - 3, y - 3))
      case Direction.UpRight => List((x + 1, y - 1), (x + 2, y - 2), (x + 3, y - 3))
      case Direction.DownLeft => List((x - 1, y + 1), (x - 2, y + 2), (x - 3, y + 3))
      case Direction.DownRight => List((x + 1, y + 1), (x + 2, y + 2), (x + 3, y + 3))

    nextCoord.zip(List('M', 'A', 'S')).forall {
      case ((x, y), char) => x >= 0 && y >= 0 && x < input.head.size && y < input.size && input(y)(x) == char
    }

  private def countXmas(x: Int, y: Int, input: List[List[Char]]): Int =
    List(
      Direction.Left,
      Direction.Right,
      Direction.Up,
      Direction.Down,
      Direction.UpLeft,
      Direction.UpRight,
      Direction.DownLeft,
      Direction.DownRight
    ).count(isXmax(x, y, input, _))

  private def isMasInX(x: Int, y: Int, input: List[List[Char]]): Boolean =
    input(y)(x) == 'A' &&
      (input(y - 1)(x - 1) == 'M' && input(y + 1)(x + 1) == 'S' ||
        input(y - 1)(x - 1) == 'S' && input(y + 1)(x + 1) == 'M') &&
      (input(y + 1)(x - 1) == 'M' && input(y - 1)(x + 1) == 'S' ||
        input(y + 1)(x - 1) == 'S' && input(y - 1)(x + 1) == 'M')


  override def part1(input: List[List[Char]]): Int =
    input.zipWithIndex.foldLeft(0) {
      case (sum, (line, y)) => line.zipWithIndex.foldLeft(sum) {
        case (sumLine, ('X', x)) => sumLine + countXmas(x, y, input)
        case (sumLine, _) => sumLine
      }
    }

  override def part2(input: List[List[Char]]): Int =
    (1 until input.size - 1).foldLeft(0)((sum, y) =>
      (1 until input(y).size - 1).foldLeft(sum)((sumLine, x) => if isMasInX(x, y, input) then sumLine + 1 else sumLine)
    )
}
