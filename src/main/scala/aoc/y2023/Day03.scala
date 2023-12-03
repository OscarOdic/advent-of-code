package aoc.y2023

object Day03 extends Puzzle2023[List[String], Int, Int] {
  val input: List[String] = getInputFile

  private case class Number(value: Int, begin: Int, end: Int)

  private def getNumbersFromList(list: String): List[Number] =
    """\d+""".r
      .findAllMatchIn(list)
      .toList
      .map(result =>
        Number(result.group(0).toInt, result.start, result.end)
      )

  private def getNumbersFromGrid(grid: List[String]): Map[Int, List[Number]] =
    grid
      .zipWithIndex
      .map { case (lineValue, lineIndex) =>
        (lineIndex, getNumbersFromList(lineValue))
      }
      .toMap

  private def isSymbol(char: Char): Boolean =
    !char.isDigit && char != '.'

  private def isAdjacent(grid: List[String], lineIndex: Int, number: Number): Boolean =
    val xMax = if (number.end < grid.head.length) number.end + 1 else number.end
    val xMin = if (number.begin > 0) number.begin - 1 else number.begin
    val adjacentPrevious = (lineIndex > 0) && grid(lineIndex - 1).substring(xMin, xMax).exists(isSymbol)
    val adjacentNext = (lineIndex < grid.length - 1) && grid(lineIndex + 1).substring(xMin, xMax).exists(isSymbol)
    val adjacentCurrent = grid(lineIndex).substring(xMin, xMax).exists(isSymbol)
    adjacentPrevious || adjacentNext || adjacentCurrent

  private def filterAdjacent(grid: List[String], numbers: Map[Int, List[Number]]): List[Number] =
    numbers
      .flatMap { case (lineIndex, lineNumbers) =>
        lineNumbers
          .filter(number => isAdjacent(grid, lineIndex, number))
      }
      .toList

  private def getGear(xGear: Int, yGear: Int, numbers: Map[Int, List[Number]]): Option[Int] =
    val adjacentNumbers = numbers
      .filter { case (y, _) =>
        List(yGear - 1, yGear, yGear + 1).contains(y)
      }
      .flatMap(_._2.filter(number =>
        (number.begin <= xGear + 1) && (number.end >= xGear)
      ))
    if (adjacentNumbers.size >= 2)
      Some(adjacentNumbers.map(_.value).product)
    else
      None

  private def getGears(grid: List[String], numbers: Map[Int, List[Number]]): List[Int] =
    grid
      .zipWithIndex
      .flatMap { case (line, y) =>
        line
          .zipWithIndex
          .filter(_._1 == '*')
          .flatMap { case (_, x) =>
            getGear(x, y, numbers)
          }
      }

  def part1(input: List[String]): Int =
    filterAdjacent(input, getNumbersFromGrid(input))
      .map(_.value)
      .sum


  def part2(input: List[String]): Int =
    getGears(input, getNumbersFromGrid(input))
      .sum
}
