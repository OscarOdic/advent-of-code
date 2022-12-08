package aoc.y2020

import scala.util.parsing.combinator.RegexParsers

object Day04 extends Puzzle2020[List[List[String]], Int, Int] with RegexParsers {
  override val input: List[List[String]] =
    getInputFile.foldLeft(List(List.empty[String])) ((list, line) =>
      if (line != "") list match {
        case h :: t => (h :+ line) :: t
      }
      else
        List.empty[String] +: list
    ).reverse

  private val requiredFields = List(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  )

  trait Field {
    val key: String
    def isValid: Boolean
  }

  case class Byr(value: Int) extends Field {
    override val key = "byr"

    override def isValid =
      value >= 1920 && value <= 2020
  }

  case class Iyr(value: Int) extends Field {
    override val key = "iyr"

    override def isValid =
      value >= 2010 && value <= 2020
  }

  case class Eyr(value: Int) extends Field {
    override val key = "eyr"

    override def isValid =
      value >= 2020 && value <= 2030
  }

  case class Hgt(value: String) extends Field {
    override val key = "hgt"

    override def isValid =
      """^(\d+)(cm|in)$""".r.findFirstMatchIn(value) match {
        case Some(matched) if matched.group(2) == "cm" =>
          matched.group(1).toInt >= 150 && matched.group(1).toInt <= 193
        case Some(matched) if matched.group(2) == "in" =>
          matched.group(1).toInt >= 59 && matched.group(1).toInt <= 76
        case _ => false
      }
  }

  case class Hcl(value: String) extends Field {
    override val key = "hcl"

    override def isValid =
      """^#[a-f0-9]{6}$""".r.findFirstIn(value).isDefined
  }

  case class Ecl(value: String) extends Field {
    override val key = "ecl"

    private val eyeColors = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

    override val isValid =
      eyeColors.contains(value)
  }

  case class Pid(value: String) extends Field {
    override val key = "pid"

    override def isValid =
      """^\d{9}$""".r.findFirstIn(value).isDefined
  }

  case class Cid(value: Int) extends Field {
    val key = "cid"

    override def isValid =
      true
  }

  private val byrParser = for {
    value <- "byr:" ~> """\d+""".r ^^ (_.toInt)
  } yield Byr(value)

  private val IyrParser = for {
    value <- "iyr:" ~> """\d+""".r ^^ (_.toInt)
  } yield Iyr(value)

  private val eyrParser = for {
    value <- "eyr:" ~> """\d+""".r ^^ (_.toInt)
  } yield Eyr(value)

  private val hgtParser = for {
    value <- "hgt:" ~> """[^:\s]+""".r
  } yield Hgt(value)

  private val hclParser = for {
    value <- "hcl:" ~> """[^:\s]+""".r
  } yield Hcl(value)

  private val eclParser = for {
    value <- "ecl:" ~> """[^:\s]+""".r
  } yield Ecl(value)

  private val pidParser = for {
    value <- "pid:" ~> """[^:\s]+""".r
  } yield Pid(value)

  private val cidParser = for {
    value <- "cid:" ~> """\d+""".r ^^ (_.toInt)
  } yield Cid(value)

  private val fieldParser =
    byrParser | IyrParser | eyrParser | hgtParser | hclParser | eclParser | pidParser | cidParser

  private val passportLineParser = rep1(fieldParser)

  override def part1(input: List[List[String]]): Int =
    input
      .map(_.flatMap(parse(passportLineParser, _).get))
      .count(passport =>
        requiredFields
          .forall(key => passport.exists(_.key == key))
      )

  override def part2(input: List[List[String]]): Int =
    input
      .map(_.flatMap(parse(passportLineParser, _).get))
      .count(passport =>
        requiredFields
          .forall(key => passport.exists(_.key == key)) &&
          passport.forall(_.isValid)
      )
}
