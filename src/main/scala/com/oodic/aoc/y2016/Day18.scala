package com.oodic.aoc.y2016

object Day18 extends Puzzle2016[String, Int, Int] {
  override val input: String = getInputFile.head

  private def getTile(line: String, pos: Int) =
    if (pos < 0 || pos >= line.length) "."
    else line(pos).toString

  private def newLine(previousLine: String) =
    (0 until previousLine.length).map(i =>
      getTile(previousLine, i-1) + getTile(previousLine, i) + getTile(previousLine, i+1) match {
        case "^^." | ".^^" | "^.." | "..^" => "^"
        case _ => "."
      }
    ).mkString

  private def nbSafeTilesLine(line: String) = line.count(_ == '.')

  private def nbSafeTilesTotal(firstLine: String, nbLines: Int) =
    (1 until nbLines).foldLeft((firstLine, nbSafeTilesLine(firstLine))) {
      case ((previousLine, nbSafeTiles), _) =>
        val line = newLine(previousLine)
        (line, nbSafeTilesLine(line) + nbSafeTiles)
    }._2

  override def resolveFirst(input: String): Int = nbSafeTilesTotal(input, 40)

  override def resolveSecond(input: String): Int = nbSafeTilesTotal(input, 400000)
}