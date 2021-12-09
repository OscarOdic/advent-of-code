package com.oodic.aoc.y2019

object Day08 extends Puzzle2019[Vector[Int], Int, String] {
  override val input: Vector[Int] = getInputFile.head.toVector.map(_.asDigit)

  private val width = 25
  private val height = 6

  private def getLayers(data: Vector[Int]): List[Vector[Int]] = input.sliding(width * height, width * height).toList

  private def fusionLayers(layers: List[Vector[Int]]): Vector[Int] = (0 until (width * height)).flatMap(index =>
    layers.map(_(index)).find(_ != 2)
  ).toVector

  private def layerToImage(layer: Vector[Int]): String = layer.map {
      case 1 => "#"
      case 0 => " "
    }
    .sliding(width, width)
    .map(_.mkString)
    .mkString("\n")

  override def part1(input: Vector[Int]): Int = {
    val layer = getLayers(input).minBy(_.count(_ == 0))
    layer.count(_ == 1) * layer.count(_ == 2)
  }

  override def part2(input: Vector[Int]): String =
    "\n" + layerToImage(fusionLayers(getLayers(input)))
}