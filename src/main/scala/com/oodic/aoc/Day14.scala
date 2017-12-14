package com.oodic.aoc

object Day14 {
  val input = getGrid("wenycdww")

  def getGrid(value: String) =
    (0 until 128).map(row =>
      Day10.knotHash(s"$value-$row")
        .map(_.toByte)
        .flatMap(byte => (0 until 8).foldLeft(List.empty[Boolean])((list, bit) =>
          (((byte >> bit) & 1) != 0) +: list
        ))
    ).toList

  def toMapCoord(value: List[List[Boolean]]) = (for {
    x <- value.indices
    y <- value(x).indices
  } yield (x,y)).filter{ case (x,y) => value(x)(y) }.map((_, 0)).toMap

  def neighbors(coords: (Int, Int), grid: Map[(Int, Int), Int]): List[(Int, Int)] =
    List(
      (coords._1 - 1, coords._2),
      (coords._1 + 1, coords._2),
      (coords._1, coords._2 - 1),
      (coords._1, coords._2 + 1)
    ).filter(neighbor => grid.exists(_._1 == neighbor) && grid(neighbor) == 0)

  def updateGroup(coords: (Int, Int), grid: Map[(Int, Int), Int], group: Int): Map[(Int, Int), Int] = {
    val updatedGrid = grid.updated(coords, group)
    neighbors(coords, updatedGrid).foldLeft(updatedGrid)((acc, neighbor) =>
      updateGroup(neighbor, acc, group)
    )
  }

  def resolveFirst(input: List[List[Boolean]]) =
    input.map(_.count(bit => bit)).sum

  def resolveSecond(input: List[List[Boolean]]) = {
    val coords = toMapCoord(input)
    def rec(grid: Map[(Int, Int), Int], group: Int = 1): Int = grid.find(_._2 == 0) match {
      case Some((coord, _)) =>
        rec(
          updateGroup(coord, grid, group),
          group + 1
        )
      case None => group
    }
    rec(coords) - 1
  }

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
