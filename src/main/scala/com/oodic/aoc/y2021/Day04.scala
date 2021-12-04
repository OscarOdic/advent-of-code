package com.oodic.aoc.y2021

import scala.annotation.tailrec

object Day04 extends Puzzle2021[(List[Int], List[List[List[Int]]]), Int, Int] {
  type Board = List[List[Int]]
  type BoardMarked = List[List[(Int, Boolean)]]

  override val input: (List[Int], List[Board]) =
    getInputFile match {
      case h :: t =>
        (h.split(',').map(_.toInt).toList, t.foldLeft(List(List.empty[List[Int]])) ((list, line) =>
          if (line != "") list match {
            case h :: t =>
              (h :+ line.trim.split("""\s+""").map(_.toInt).toList) :: t
          }
          else
            List.empty[List[Int]] +: list
        ).reverse)
    }

  private def toBoardMarked(board: Board): BoardMarked =
    board.map(_.map((_, false)))

  private def boardWin(boardMarked: BoardMarked): Boolean =
    boardMarked.exists(_.forall(_._2)) || boardMarked.transpose.exists(_.forall(_._2))

  private def finished(boards: List[BoardMarked]): Option[BoardMarked] =
    boards
      .find(boardWin)

  private def getUnMarked(board: BoardMarked): List[Int] =
    board.flatMap(_.filter(!_._2)).map(_._1)

  private def execute(n: Int, boards: List[BoardMarked]): List[BoardMarked] =
    boards.map(_.map(_.map {
      case (x, _) if x == n => (x, true)
      case x => x
    }))

  @tailrec
  private def firstWin(numbers: List[Int], boards: List[BoardMarked]): Int =
    numbers match {
      case n :: others =>
        val nextBoards = execute(n, boards)
        finished(nextBoards) match {
          case Some(boardWin) =>
            n * getUnMarked(boardWin).sum
          case _ => firstWin(others, nextBoards)
        }
    }

  @tailrec
  private def lastWin(numbers: List[Int], boards: List[BoardMarked], nWin: Option[Int] = None, lastBoard: Option[BoardMarked] = None): Int =
    numbers match {
      case n :: others =>
        val nextBoards = execute(n, boards)
        finished(nextBoards) match {
          case Some(lastBoardWin) if nextBoards.size == 1 =>
            n * getUnMarked(lastBoardWin).sum
          case Some(board) =>
            lastWin(others, nextBoards.filter(!boardWin(_)), Some(n), Some(board))
          case _ => lastWin(others, nextBoards, nWin, lastBoard)
        }
      case _ => (for {
        n <- nWin
        board <- lastBoard
      } yield n * getUnMarked(board).sum).getOrElse(0)
    }

  override def resolveFirst(input: (List[Int], List[Board])): Int =
    firstWin(input._1, input._2.map(toBoardMarked))

  override def resolveSecond(input: (List[Int], List[Board])): Int =
    lastWin(input._1, input._2.map(toBoardMarked))
}
