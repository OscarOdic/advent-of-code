package com.oodic.aoc.y2019
import scala.collection.immutable.Queue

object Day11 extends Puzzle2019[Vector[Long], Int, String] {
  override val input: Vector[Long] = getInputFile.head.split(",").toVector.map(_.toLong)

  trait ParamMode
  object Position extends ParamMode
  object Immediate extends ParamMode
  object Relative extends ParamMode

  trait Mode
  object Running extends Mode
  object Halt extends Mode
  object Terminated extends Mode

  trait Direction
  object Up extends Direction
  object Down extends Direction
  object Right extends Direction
  object Left extends Direction

  case class State(program: Vector[Long], index: Long = 0, inputs: Queue[Long], outputs: Queue[Long] = Queue.empty, relativeBase: Long = 0)

  case class Instruction(instruction: Long, param1: ParamMode, param2: ParamMode, param3: ParamMode)

  private def parseInstruction(value: Long) =
    if (value <= 99) Instruction(value, Position, Position, Immediate)
    else Instruction(
      value % 100,
      if (value/100 % 10 == 0) Position else if (value/100 % 10 == 1) Immediate else Relative,
      if (value/1000 % 10 == 0) Position else if (value/1000 % 10 == 1) Immediate else Relative,
      if (value/10000 % 10 == 0) Position else if (value/10000 % 10 == 1) Immediate else Relative
    )

  private def readFromPosition(program: Vector[Long], pos: Int): Long =
    if (pos > program.size - 1) 0L
    else program(pos)

  private def getParam(mode: ParamMode, value: Long, input: Vector[Long], relativeBase: Long): Long = mode match {
    case Position => readFromPosition(input, input(value.toInt).toInt)
    case Immediate => input(value.toInt)
    case Relative => readFromPosition(input, input(value.toInt).toInt + relativeBase.toInt)
  }

  private def writeParam(mode: ParamMode, value: Long, input: Vector[Long], relativeBase: Long): Long = mode match {
    case Immediate | Position => input(value.toInt)
    case Relative => input(value.toInt) + relativeBase
  }

  private def updateWithSize(vector: Vector[Long], index: Int, value: Long): Vector[Long] = {
    val newVector = if (index > vector.size - 1) vector ++ Vector.fill(index + 1 - vector.size)(0L)
    else vector
    newVector.updated(index, value)
  }

  private def execute(state: State): (Mode, State) = parseInstruction(state.program(state.index.toInt)) match {
    case Instruction(instruction, param1, param2, param3) => instruction match {
      case 1 =>
        execute(state.copy(program = updateWithSize(state.program, writeParam(param3, state.index + 3, state.program, state.relativeBase).toInt, getParam(param1, state.index + 1, state.program, state.relativeBase) + getParam(param2, state.index + 2, state.program, state.relativeBase)), index = state.index + 4))
      case 2 =>
        execute(state.copy(program = updateWithSize(state.program, writeParam(param3, state.index + 3, state.program, state.relativeBase).toInt, getParam(param1, state.index + 1, state.program, state.relativeBase) * getParam(param2, state.index + 2, state.program, state.relativeBase)), index = state.index + 4))
      case 3 =>
        if (state.inputs.isEmpty) (Halt, state)
        else {
          val (first, newInputs) = state.inputs.dequeue
          execute(state.copy(program = updateWithSize(state.program, writeParam(param1, state.index + 1, state.program, state.relativeBase).toInt, first), index = state.index + 2, inputs = newInputs))
        }
      case 4 =>
        val newOutputs = state.outputs.enqueue(getParam(param1, state.index + 1, state.program, state.relativeBase))
        execute(state.copy(index = state.index + 2, outputs = newOutputs))
      case 5 =>
        getParam(param1, state.index + 1, state.program, state.relativeBase) match {
          case 0 => execute(state.copy(program = state.program, index = state.index + 3))
          case _ => execute(state.copy(program = state.program, index = getParam(param2, state.index + 2, state.program, state.relativeBase)))
        }
      case 6 =>
        getParam(param1, state.index + 1, state.program, state.relativeBase) match {
          case 0 => execute(state.copy(program = state.program, index = getParam(param2, state.index + 2, state.program, state.relativeBase)))
          case _ => execute(state.copy(program = state.program, index = state.index + 3))
        }
      case 7 =>
        (getParam(param1, state.index + 1, state.program, state.relativeBase), getParam(param2, state.index + 2, state.program, state.relativeBase)) match {
          case (left, right) if left < right =>
            execute(state.copy(program = updateWithSize(state.program, writeParam(param3, state.index + 3, state.program, state.relativeBase).toInt, 1L), index = state.index + 4))
          case _ =>
            execute(state.copy(program = state.program.updated(writeParam(param3, state.index + 3, state.program, state.relativeBase).toInt, 0L), index = state.index + 4))
        }
      case 8 =>
        (getParam(param1, state.index + 1, state.program, state.relativeBase), getParam(param2, state.index + 2, state.program, state.relativeBase)) match {
          case (left, right) if left == right =>
            execute(state.copy(program = updateWithSize(state.program, writeParam(param3, state.index + 3, state.program, state.relativeBase).toInt, 1L), index = state.index + 4))
          case _ =>
            execute(state.copy(program = updateWithSize(state.program, writeParam(param3, state.index + 3, state.program, state.relativeBase).toInt, 0L), index = state.index + 4))
        }
      case 9 =>
        execute(state.copy(index = state.index + 2, relativeBase = state.relativeBase + getParam(param1, state.index + 1, state.program, state.relativeBase)))
      case 99 => (Terminated, state)
    }
  }

  private def paint(inputs: Queue[Long], position: (Int, Int), direction: Direction, panels: Map[(Int, Int), Boolean]): (Map[(Int, Int), Boolean], (Int, Int), Direction) =
    if (inputs.isEmpty) (panels, position, direction)
    else {
      val (color, inputsTmp) = inputs.dequeue
      val (newDir, nextInputs) = inputsTmp.dequeue
      val white = if (color == 0L) false else true
      val newDirection = direction match {
        case Up if newDir == 0L => Left
        case Up => Right
        case Down if newDir == 0L => Right
        case Down => Left
        case Right if newDir == 0L => Up
        case Right => Down
        case Left if newDir == 0L => Down
        case Left => Up
      }
      val newPosition = newDirection match {
        case Up => position.copy(_2 = position._2 + 1)
        case Down => position.copy(_2 = position._2 - 1)
        case Right => position.copy(_1 = position._1 + 1)
        case Left => position.copy(_1 = position._1 - 1)
      }
      paint(nextInputs, newPosition, newDirection, panels.updated(position, white))
    }

  private def paintingRobot(state: State, mode: Mode = Running, panels: Map[(Int, Int), Boolean] = Map.empty, position: (Int, Int) = (0, 0), direction: Direction = Up, start: Boolean = false): Map[(Int, Int), Boolean] =
    if (mode == Terminated) panels
    else {
      val (newMode, newState) = execute(state.copy(inputs = Queue(if (panels.getOrElse(position, false)) 1 else 0)))
      val (newPanel, newPosition, newDirection) = paint(newState.outputs, position, direction, panels)
      val colorPanel = newPanel.getOrElse(newPosition, false) match {
        case false => 0L
        case true => 1L
      }
      paintingRobot(newState.copy(inputs = Queue(colorPanel), outputs = Queue.empty), newMode, newPanel, newPosition, newDirection)
    }

  override def part1(input: Vector[Long]): Int = paintingRobot(State(program = input, inputs = Queue(0))).size

  override def part2(input: Vector[Long]): String = {
    val painting = paintingRobot(State(program = input, inputs = Queue(1)), panels = Map((0, 0) -> true), start = true)
      .mapValues(if (_) "#" else ".")
    val ymin = painting.map(_._1._2).min
    val ymax = painting.map(_._1._2).max
    val xmin = painting.map(_._1._1).min
    val xmax = painting.map(_._1._1).max
    (ymax to ymin by -1).map(y =>
      "\n" + (xmin to xmax).map(x => painting.getOrElse((x, y), ".")).mkString
    ).mkString
  }
}
