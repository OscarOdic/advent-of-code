package com.oodic.aoc.y2019

object Day06 extends Puzzle2019[Vector[String], Int, Int] {
  override val input: Vector[String] = getInputFile.toVector

  private def parseEdge(value: String): (String, String) = value.split("""\)""") match {
    case Array(left, right) => (left, right)
  }

  private def countOrbits(edges: Vector[(String, String)], obj: String): Int = {
    val directs = edges.filter(_._1 == obj)
    directs.map(o => countOrbits(edges, o._2)).sum + directs.size
  }

  private def pathBetweenTwoObjects(edges: Vector[(String, String)], obj1: String, obj2: String): Vector[String] =
    if (obj1 == obj2) Vector(obj2)
    else pathBetweenTwoObjects(edges, obj1, edges.filter(_._2 == obj2).head._1) :+ obj2

  private def distancePath(path: List[String], root: String) = {
    def rec(pathTmp: List[String] = path): List[String] =
      if (pathTmp.head == root) pathTmp
      else rec(pathTmp.tail)
    rec().size
  }

  private def distanceBetweenTwoObjects(edges: Vector[(String, String)], obj1: String, obj2: String, root: String): Int = {
    val pathObj1 = pathBetweenTwoObjects(edges, root, obj1)
    val pathObj2 = pathBetweenTwoObjects(edges, root, obj2)
    val rootObjs = (pathObj1 intersect pathObj2).reverse.head
    distancePath(pathObj1.toList, rootObjs) + distancePath(pathObj2.toList, rootObjs) - 4
  }

  override def resolveFirst(input: Vector[String]): Int = {
    val edges = input.map(parseEdge)
    val objects = edges.map(_._1).distinct
    objects.map(countOrbits(edges, _)).sum
  }

  override def resolveSecond(input: Vector[String]): Int = distanceBetweenTwoObjects(input.map(parseEdge), "YOU", "SAN", "COM")
}
