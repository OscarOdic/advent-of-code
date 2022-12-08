package aoc.y2017

import scala.util.parsing.combinator.RegexParsers

object Day20 extends Puzzle2017[List[String], Int, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  case class Particle(position: (Int, Int, Int), velocity: (Int, Int, Int), acceleration: (Int, Int, Int)) {
    def execute: Particle = {
      val newVelocity = (velocity._1 + acceleration._1, velocity._2 + acceleration._2, velocity._3 + acceleration._3)
      val newPosition = (position._1 + newVelocity._1, position._2 + newVelocity._2, position._3 + newVelocity._3)
      Particle(newPosition, newVelocity, acceleration)
    }
  }

  val coordParser: Parser[(Int, Int, Int)] = for {
    _ <- "<".r
    x <- """-?\d+""".r <~ "," ^^ (_.toInt)
    y <- """-?\d+""".r <~ "," ^^ (_.toInt)
    z <- """-?\d+""".r ^^ (_.toInt)
    _ <- ">".r
  } yield (x,y,z)

  val particleParser: Parser[Particle] = for {
    p <- "p=" ~> coordParser <~ ","
    v <- "v=" ~> coordParser <~ ","
    a <- "a=" ~> coordParser
  } yield Particle(p,v,a)

  private def sumTuple(coord: (Int, Int, Int)) = coord match {
    case (x,y,z) => math.abs(x) + math.abs(y) + math.abs(z)
  }

  override def part1(input: List[String]): Int =
    input.map(parse(particleParser, _).get)
      .map { case Particle(p,v,a) => (sumTuple(a), sumTuple(v), sumTuple(p)) }
      .zipWithIndex
      .min
      ._2

  private def executeList(particles: List[(Particle, Int)]) =
    particles.map { case (particle, id) => (particle.execute, id) }

  override def part2(input: List[String]): Int =
    (0 until 100).foldLeft(input.map(parse(particleParser, _).get).zipWithIndex)((particles, _) => {
      val filtredParticles = particles.foldLeft(List.empty[(Particle, Int)])((list, particle) =>
        if (particles.exists(p => p._1.position == particle._1.position && p._2 != particle._2 )) list
        else particle +: list
      )
      executeList(filtredParticles)
    }).size
}
