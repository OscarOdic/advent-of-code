package com.oodic.aoc

object Day14 extends PuzzleDay[List[List[Boolean]], Int, Int] {
  override val input: List[List[Boolean]] = getGrid("wenycdww")

  def getGrid(value: String): List[List[Boolean]] =
    (0 until 128).map(row =>
      Day10.knotHash(s"$value-$row")
        .map(_.toByte)
        .flatMap(byte => (0 until 8).foldLeft(List.empty[Boolean])((list, bit) =>
          (((byte >> bit) & 1) != 0) +: list
        ))
    ).toList

  def toMapCoord(value: List[List[Boolean]]): Map[(Int, Int), Int] = (for {
    x <- value.indices
    y <- value(x).indices
  } yield (x,y)).filter{ case (x,y) => value(x)(y) }.map((_, 0)).toMap

  def neighbors(coord: (Int, Int), grid: Map[(Int, Int), Int]): List[(Int, Int)] =
    List(
      (coord._1 - 1, coord._2),
      (coord._1 + 1, coord._2),
      (coord._1, coord._2 - 1),
      (coord._1, coord._2 + 1)
    ).filter(neighbor => grid.exists(_._1 == neighbor) && grid(neighbor) == 0)

  def updateGroup(coord: (Int, Int), grid: Map[(Int, Int), Int], group: Int): Map[(Int, Int), Int] = {
    val updatedGrid = grid.updated(coord, group)
    neighbors(coord, updatedGrid).foldLeft(updatedGrid)((acc, neighbor) =>
      updateGroup(neighbor, acc, group)
    )
  }

  override def resolveFirst(input: List[List[Boolean]]): Int =
    input.map(_.count(bit => bit)).sum

  override def resolveSecond(input: List[List[Boolean]]): Int = {
    def rec(grid: Map[(Int, Int), Int], group: Int = 1): Int = grid.find(_._2 == 0) match {
      case Some((c, _)) =>
        rec(
          updateGroup(c, grid, group),
          group + 1
        )
      case None => group
    }
    rec(toMapCoord(input)) - 1
  }
}
