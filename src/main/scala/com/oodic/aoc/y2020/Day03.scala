package com.oodic.aoc.y2020

object Day03 extends Puzzle2020[Vector[Vector[Char]], Int, Int] {
  override val input: Vector[Vector[Char]] =
    getInputFile.map(_.toVector).toVector

  private def parseMap(input: Vector[Vector[Char]]): Vector[Vector[Boolean]] =
    input.map(_.map(_ == '#'))

  private def nextPosition(move: (Int, Int) = (3, 1)) =
    (pos: (Int, Int)) => (pos._1 + move._1, pos._2 + move._2)

  private def countTreesOnToboggan(map: Vector[Vector[Boolean]], pos: (Int, Int) = (0, 0))
                                  (next: ((Int, Int)) => (Int, Int) = nextPosition()): Int =
    if (map.size <= pos._2)
      0
    else
      (if (map(pos._2)(pos._1 % map(0).size)) 1 else 0) + countTreesOnToboggan(map, next(pos))(next)

  override def part1(input: Vector[Vector[Char]]): Int =
    countTreesOnToboggan(parseMap(input))()

  override def part2(input: Vector[Vector[Char]]): Int =
    List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    ).map(move =>
      countTreesOnToboggan(parseMap(input))(nextPosition(move))
    ).product
}
