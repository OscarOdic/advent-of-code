package aoc.y2023

import scala.annotation.tailrec

object Day09 extends Puzzle2023[List[List[Int]], Int, Int]{
  override val input: List[List[Int]] = getInputFile
    .map(_.split(" ").toList.map(_.toInt))

  @tailrec
  private def getTree(currentTree: List[List[Int]]): List[List[Int]] =
    currentTree.head match
      case line if line.forall(_ == 0) =>
        currentTree
      case line =>
        val newLine = line.tail
          .foldLeft((List.empty[Int], line.head)) {
            case ((accLine, previousValue), nextValue) =>
              (accLine :+ (previousValue - nextValue), nextValue)
          }._1
        getTree(newLine +: currentTree)

  private def nextValueFistHistory(tree: List[List[Int]]): Int =
    tree.tail.foldLeft(0)((result, line) =>
      result + line.head
    )

  private def resolve(input: List[List[Int]]): Int =
    input
      .map(line => getTree(List(line)))
      .map(nextValueFistHistory)
      .sum

  override def part1(input: List[List[Int]]): Int =
    resolve(input.map(_.reverse))

  override def part2(input: List[List[Int]]): Int =
    resolve(input)
}
