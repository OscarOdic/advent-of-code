package aoc.y2021

object Day10 extends Puzzle2021[List[String], Int, Long] {
  override val input: List[String] =
    getInputFile

  trait StatusLine
  object SuccessLine extends StatusLine
  case class IncompleteLine(chars: List[Char]) extends StatusLine
  case class FailureLine(char: Char) extends StatusLine

  private def getStatusLine(line: List[Char], stack: List[Char] = List.empty): StatusLine =
    line match {
      case h :: t =>
        h match {
          case '(' | '[' | '{' | '<' =>
            getStatusLine(t, h +: stack)
          case ')' if stack.headOption.contains('(') =>
            getStatusLine(t, stack.tail)
          case ']' if stack.headOption.contains('[') =>
            getStatusLine(t, stack.tail)
          case '}' if stack.headOption.contains('{') =>
            getStatusLine(t, stack.tail)
          case '>' if stack.headOption.contains('<') =>
            getStatusLine(t, stack.tail)
          case _ =>
            FailureLine(h)
        }
      case _ if stack.isEmpty => SuccessLine
      case _ => IncompleteLine(stack)
    }

  private def getScore(line: List[Char]) =
    line.foldLeft(0L)((currentScore, char) =>
      char match {
        case '(' => 1L + (currentScore * 5L)
        case '[' => 2L + (currentScore * 5L)
        case '{' => 3L + (currentScore * 5L)
        case '<' => 4L + (currentScore * 5L)
      }
    )

  override def part1(input: List[String]): Int =
    input
      .map(line => getStatusLine(line.toList))
      .map {
        case FailureLine(')') => 3
        case FailureLine(']') => 57
        case FailureLine('}') => 1197
        case FailureLine('>') => 25137
        case _ => 0
      }.sum

  override def part2(input: List[String]): Long = {
    val incompleteLines = input
      .map(line => getStatusLine(line.toList))
      .map {
        case IncompleteLine(chars) => getScore(chars)
        case _ => 0
      }.filter(_ != 0)
      .sorted
    incompleteLines(incompleteLines.size / 2)
  }
}
