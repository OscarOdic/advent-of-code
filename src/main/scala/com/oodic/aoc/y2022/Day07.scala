package com.oodic.aoc.y2022

import scala.annotation.tailrec

object Day07 extends Puzzle2022[List[String], Int, Int] {
  override val input: List[String] = getInputFile

  private def updatePath(currentPath: String, dir: String): String =
    dir match
      case "/" =>
        "/"
      case ".." =>
        """(^.*/)[^/]+/$""".r
          .findFirstMatchIn(currentPath)
          .get
          .group(1)
      case value =>
        s"$currentPath$value/"

  private def getSizeFromLs(lsOutput: List[String]): Int =
    lsOutput
      .map("""\d+""".r.findFirstIn(_).map(_.toInt).getOrElse(0))
      .sum

  @tailrec
  private def execute(
                       terminalOutput: List[String],
                       currentPath: String = "/",
                       directories: Map[String, Int] = Map.empty
                     ): Map[String, Int] = {
    val currentDirectories =
      if (!directories.contains(currentPath)) directories.updated(currentPath, 0)
      else directories

    terminalOutput match
      case s"$$ cd $dir" :: tail => execute(tail, updatePath(currentPath, dir), currentDirectories)
      case s"$$ ls" :: tail =>
        val lsOutput = tail.takeWhile(!_.startsWith("$"))
        val nextTerminalOutput = tail.dropWhile(!_.startsWith("$"))
        execute(nextTerminalOutput, currentPath, currentDirectories.updated(currentPath, getSizeFromLs(lsOutput)))
      case _ => currentDirectories
  }

  private def sizeDir(dir: String, dirs: Map[String, Int]): Int =
    dirs(dir) + dirs
      .filter(subDir => s"""^%s.+""".format(dir).r.matches(subDir._1))
      .values
      .sum

  override def part1(input: List[String]): Int =
    val dirs = execute(input)
    dirs
      .map(dir => sizeDir(dir._1, dirs))
      .filter(_ <= 100000)
      .sum

  override def part2(input: List[String]): Int =
    val dirs = execute(input)
    val sizes = dirs
      .map(dir => sizeDir(dir._1, dirs))
    sizes
      .filter(_ >= sizes.max - 40000000)
      .min
}
