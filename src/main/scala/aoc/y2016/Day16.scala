package aoc.y2016

object Day16 extends Puzzle2016[String, String, String] {
  override val input: String = "10001110011110000"

  def checkSum(value: String): String = {
    val newValue = value.sliding(2, 2)
      .map(pair => if (pair(0) == pair(1)) '1' else '0')
      .mkString
    if (newValue.length % 2 == 0) checkSum(newValue)
    else newValue
  }

  def fill(value: String, n: Int): String = {
    if (value.length >= n) value.take(n)
    else fill(value + 0 + value.reverse.map(char => if (char == '1') '0' else '1'), n)
  }

  override def part1(input: String): String =
    checkSum(fill(input, 272))

  override def part2(input: String): String =
    checkSum(fill(input, 35651584))
}