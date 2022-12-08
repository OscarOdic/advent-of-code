package aoc.y2021

import scala.annotation.tailrec

object Day17 extends Puzzle2021[((Int, Int), (Int, Int)), Int, Int] {
  override val input: ((Int, Int), (Int, Int)) =
    ((207, 263), (-115, -63))

  @tailrec
  private def reachTarget(velocity: (Int, Int), target: ((Int, Int), (Int, Int)), pos: (Int, Int) = (0, 0)): Boolean =
    if (pos._1 >= target._1._1 && pos._1 <= target._1._2 && pos._2 >= target._2._1 && pos._2 <= target._2._2)
      true
    else if (pos._1 > target._1._2 || pos._2 < target._2._1)
      false
    else
      (velocity, pos) match {
        case ((vx, vy), (x, y)) =>
          val nextVelocity = (if (vx > 0) vx - 1 else if (vx < 0) vx + 1 else 0, vy - 1)
          val nexPos = (x + vx, y + vy)
          reachTarget(nextVelocity, target, nexPos)
      }


  override def part1(input: ((Int, Int), (Int, Int))): Int =
    math.abs(input._2._1) * (math.abs(input._2._1) - 1) / 2

  override def part2(input: ((Int, Int), (Int, Int))): Int =
    (for {
      vx <- math.sqrt(input._1._1 * 2).toInt to input._1._2 + 1
      vy <- input._2._1 to math.abs(input._2._1)
    } yield (vx, vy))
      .count(v =>
        reachTarget(v, input)
      )
}