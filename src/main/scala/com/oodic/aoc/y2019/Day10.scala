package com.oodic.aoc.y2019

object Day10 extends Puzzle2019[Vector[Vector[String]], Int, Int] {
  override val input: Vector[Vector[String]] = getInputFile.toVector.map(_.toVector.map(_.toString))

  private case class Position(x:Int, y:Int) {
    def addDir(other: Direction) = Position(x-other.x, y-other.y)
    def removePos(other: Position) = Direction(x-other.x, y-other.y)
  }

  private case class Direction(x:Int, y:Int) {
    def length: Double = scala.math.sqrt(x*x+y*y)
    def angle: Double = {
      val ratio = x / length
      if (x < 0 && y >= 0) -ratio else if (x <= 0) ratio+2 else if (y < 0) ratio+2 else 4-ratio
    }
    override def hashCode: Int = ((x/length*10).round,(y/length*10).round).##
    override def equals(other:Any): Boolean = other match {
      case o: Direction => (x * o.length - length * o.x).abs < 0.1 && (y * o.length - length * o.y).abs < 0.1
      case _ => false
    }
  }

  private def findAsteroids(origin: Position, asteroids: Set[Position], widthLength: Int, heightLength: Int): Set[Direction] = (for {
    x <- 0 until widthLength
    y <- 0 until heightLength
    asteroid = Position(x,y)
    if asteroid != origin && (asteroids contains asteroid)
  } yield origin.removePos(asteroid))
    .sortBy(_.length).toSet

  private def getAsteroids(input: Vector[Vector[String]]) = (for {
    x <- input(0).indices
    y <- input.indices
    if input(y)(x) == "#"
  } yield Position(x,y)).toSet

  private def getStation(asteroids: Set[Position], width: Int, height: Int) =
    asteroids.map(asteroid =>
      (asteroid, findAsteroids(asteroid, asteroids, width, height).size)
    ).maxBy(_._2)

  override def resolveFirst(input: Vector[Vector[String]]): Int = {
    val asteroids = getAsteroids(input)
    getStation(asteroids, input(0).length, input.length)._2
  }

  override def resolveSecond(input: Vector[Vector[String]]): Int = {
    val asteroids = getAsteroids(input)
    val station = getStation(asteroids, input(0).length, input.length)._1
    val asteroid200 = station.addDir(findAsteroids(station, asteroids, input(0).length, input.length).toArray.sortBy(_.angle).drop(199).head)
    asteroid200.x * 100 + asteroid200.y
  }
}
