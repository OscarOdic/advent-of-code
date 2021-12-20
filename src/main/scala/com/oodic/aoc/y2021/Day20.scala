package com.oodic.aoc.y2021

object Day20 extends Puzzle2021[(String, Map[(Int, Int), Char]), Int, Int] {
  override val input: (String, Map[(Int, Int), Char]) =
    getInputFile match {
      case h :: _ :: t => (h,
        (for {
          (line, y) <- t.zipWithIndex
          (value, x) <- line.zipWithIndex
        } yield (x, y) -> value).toMap)
    }

  private def neighbors(pixel: (Int, Int)): List[(Int, Int)] =
    pixel match {
      case (x, y) => List(
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
        (x - 1, y),
        (x, y),
        (x + 1, y),
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1)
      )
    }

  private def getBinaryNumber(pixel: (Int, Int), image: Map[(Int, Int), Char], valueOuter: Int): String =
    neighbors(pixel).map(pixel =>
      image.get(pixel) match {
        case Some('#') => 1
        case Some('.') => 0
        case _ => valueOuter
      }
    ).mkString

  private def decryptChar(binary: String, algorithm: String) =
    algorithm(Integer.parseInt(binary, 2))

  private def execute(image: Map[(Int, Int), Char], algorithm: String, valueOuter: Int = 0): Map[(Int, Int), Char] =
    (for {
      y <- image.keys.map(_._2).min - 1 to image.keys.map(_._2).max + 1
      x <- image.keys.map(_._1).min - 1 to image.keys.map(_._1).max + 1
    } yield (x, y)).map(pos =>
        (pos, decryptChar(
          getBinaryNumber(pos, image, valueOuter),
          algorithm
        ))
    ).toMap

  private def executeTimes(image: Map[(Int, Int), Char], algorithm: String, times: Int = 2): Map[(Int, Int), Char] =
    if (times == 0) image
    else
      executeTimes(
        execute(image, algorithm, if (times % 2 == 1 && algorithm(0) == '#') 1 else 0),
        algorithm,
        times - 1
      )

  override def part1(input: (String, Map[(Int, Int), Char])): Int =
    executeTimes(input._2, input._1).count(_._2 == '#')

  override def part2(input: (String, Map[(Int, Int), Char])): Int =
    executeTimes(input._2, input._1, 50).count(_._2 == '#')
}
