package com.oodic.aoc.y2016

object Day13 extends Puzzle2016[Int, Int, Int] {
  override val input: Int = 1364

  private def isOpen(x: Int, y: Int, salt:Int) =
    x >= 0 && y >= 0 && (x*x + 3*x + 2*x*y + y + y*y + salt)
    .toBinaryString.count(_ == '1') % 2 == 0

  private def next(p: (Int, Int), salt: Int) =
    List((p._1, p._2+1), (p._1, p._2-1), (p._1+1, p._2), (p._1-1, p._2))
    .filter(value => isOpen(value._1, value._2, salt))

  private def reachPosition(
                             target: (Int, Int),
                             salt: Int,
                             positions: Set[(Int, Int)] = Set((0,0)),
                             visited: Set[(Int, Int)] = Set(),
                             nbSteps: Int = 0,
                           ): Int =
    if (positions.contains(target)) nbSteps
    else reachPosition(
      target,
      salt,
      positions.flatMap(next(_, salt)) -- visited,
      visited ++ positions,
      nbSteps+1
    )

  private def getPositions(
                            salt: Int,
                            positions: Set[(Int, Int)] = Set((0,0)),
                            visited: Set[(Int, Int)] = Set(),
                            nbSteps: Int = 50
                          ): Int =
    if (nbSteps == 0) visited.size
    else getPositions(
      salt,
      positions.flatMap(next(_, salt)) -- visited,
      visited ++ positions,
      nbSteps-1
    )

  override def part1(input: Int): Int = reachPosition((31, 39), input)

  override def part2(input: Int): Int = getPositions(input)
}