package com.oodic.aoc.y2020

import scala.util.parsing.combinator.RegexParsers

object Day18 extends Enumeration with Puzzle2020[List[String], Long, Long] with RegexParsers {
  override val input: List[String] =
    getInputFile

  type Operator = Value
  val Sum, Product = Value

  private def evaluateEither(value: Either[Long, OperationSimple]): Long =
    value match {
      case Left(value) => value
      case Right(value) => value.evaluate
    }

  case class OperationSimple(left: Either[Long, OperationSimple], right: List[(Operator, Either[Long, OperationSimple])] = Nil) {
    def evaluate: Long = right.foldLeft(evaluateEither(left)) {
      case (previousOperation, (nextOperator, nextOperation)) =>
        nextOperator match {
          case Sum => previousOperation + evaluateEither(nextOperation)
          case Product => previousOperation * evaluateEither(nextOperation)
        }
    }
  }

  private def parseEither: Parser[Either[Long, OperationSimple]] =
    (("""\d+""".r ^^ (_.toLong)) | ("(" ~> parseOperation <~ ")")) ^^ {
      case value: Long => Left(value)
      case value: OperationSimple => Right(value)
    }

  private def parseOperation: Parser[OperationSimple] = for {
    left <- parseEither
    right <- rep(for {
      operator <- ("+" | "*") map {
        case "+" => Sum
        case "*" => Product
      }
      operation <- parseEither
    } yield (operator, operation))
  } yield OperationSimple(left, right)

  override def resolveFirst(input: List[String]): Long =
    input
      .map(parse(parseOperation, _).get.evaluate).sum

  private def evaluateEitherComplex(value: Either[Long, OperationComplex]): Long =
    value match {
      case Left(value) => value
      case Right(value) => value.evaluate
    }

  case class OperationComplex(left: Either[Long, OperationComplex], operator: Operator, right: Either[Long, OperationComplex]) {
    def evaluate: Long =
      operator match {
        case Sum => evaluateEitherComplex(left) + evaluateEitherComplex(right)
        case Product => evaluateEitherComplex(left) * evaluateEitherComplex(right)
      }
    override def toString: String = {
      val leftString = left match {
        case Left(i) => i.toString
        case Right(value) => s"(${value.toString})"
      }
      val operatorString = operator match {
        case Sum => "+"
        case Product => "*"
      }
      val rightString = right match {
        case Left(i) => i.toString
        case Right(value) => s"(${value.toString})"
      }
      s"$leftString $operatorString $rightString"
    }
  }

  private def parseEitherComplex: Parser[Either[Long, OperationComplex]] =
    (("""\d+""".r ^^ (_.toLong)) | ("(" ~> parseOperationComplex <~ ")")) ^^ {
      case value: Long => Left(value)
      case value: Either[Long, OperationComplex] => value
    }

  private def parseSumComplex: Parser[OperationComplex] = {
    for {
      left <- parseEitherComplex <~ "+"
      right <- parseSumComplex ^^ (Right(_)) | parseEitherComplex
    } yield OperationComplex(left, Sum, right)
  }

  private def parseProductComplex: Parser[OperationComplex] = {
    for {
      left <- (parseSumComplex ^^ (Right(_)) | parseEitherComplex) <~ "*"
      right <- parseOperationComplex
    } yield OperationComplex(left, Product, right)
  }

  private def parseOperationComplex: Parser[Either[Long, OperationComplex]] =
    parseProductComplex ^^ (Right(_)) | parseSumComplex ^^ (Right(_)) | parseEitherComplex

  override def resolveSecond(input: List[String]): Long =
    input.map(parse(parseOperationComplex, _).get.right.get.evaluate).sum
}
