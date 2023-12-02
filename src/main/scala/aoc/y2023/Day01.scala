package aoc.y2023

object Day01 extends Puzzle2023[List[String], Int, Int] {
  override val input: List[String] = getInputFile

  private val digitLetters = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9'
  )

  private def toDigits(s: String): String =
    s.headOption match
      case Some(c) =>
        if (c.isDigit) c +: toDigits(s.tail)
        else digitLetters.find { case (letters, _) => s.startsWith(letters) } match
          case Some(_, digit) => digit +: toDigits(s.tail)
          case _ => toDigits(s.tail)
      case _ => s

  override def part1(input: List[String]): Int =
    input.map { line =>
      val digitLine = line.filter(_.isDigit)
      f"${digitLine.head}${digitLine.last}".toInt
    }.sum

  override def part2(input: List[String]): Int =
    input.map { line =>
      val digitLine = toDigits(line)
      f"${digitLine.head}${digitLine.last}".toInt
    }.sum
}
