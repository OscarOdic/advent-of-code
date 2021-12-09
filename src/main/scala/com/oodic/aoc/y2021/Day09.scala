package com.oodic.aoc.y2021

object Day09 extends Puzzle2021[Map[(Int, Int), Int], Int, Int] {
  override val input: Map[(Int, Int), Int] =
    (for {
      (line, y) <- getInputFile.zipWithIndex
      (value, x) <- line.zipWithIndex
    } yield (x, y) -> value.asDigit)
      .toMap

  private def neighbors(x: Int, y: Int): List[(Int, Int)] =
    List((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))

  private def isLow(x: Int, y: Int, heightMap: Map[(Int, Int), Int]): Boolean =
    heightMap.get((x, y)) match {
      case Some(value) =>
        neighbors(x, y).forall(neighbor =>
          heightMap.get(neighbor) match {
            case Some(neighborValue) =>
              value < neighborValue
            case _ => true
          }
        )
      case _ => false
    }

  private def lowNumbers(heightMap: Map[(Int, Int), Int]): List[((Int, Int), Int)] =
    heightMap
      .filterKeys {
        case (x, y) => isLow(x, y, heightMap)
      }.toList

  private def getBasin(x: Int, y: Int, heightMap: Map[(Int, Int), Int], visited: List[(Int, Int)] = List.empty): (List[(Int, Int)], List[(Int, Int)]) =
    heightMap.get((x, y)) match {
      case Some(9) | None =>
        ((x, y) +: visited, List.empty)
      case _ =>
        val nextVisited = (x, y) +: visited
        val n = neighbors(x, y).diff(nextVisited)
        n.foldLeft((nextVisited, List((x, y)))) {
          case ((currentVisited, currentBasin), (nX, nY)) =>
            val (nV, nB) = getBasin(nX, nY, heightMap, currentVisited)
            (nV, (currentBasin ++ nB).distinct)
        }
    }

  private def getBasinSizes(heightMap: Map[(Int, Int), Int]): List[Int] =
    lowNumbers(heightMap)
      .map {
        case ((x, y), _) => getBasin(x, y, heightMap, List((x, y)))._2.size
      }

  override def part1(input: Map[(Int, Int), Int]): Int =
    lowNumbers(input)
      .map(_._2 + 1)
      .sum

  override def part2(input: Map[(Int, Int), Int]): Int =
    getBasinSizes(input)
      .sorted(Ordering.Int.reverse)
      .take(3)
      .product
}
