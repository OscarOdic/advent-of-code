package com.oodic.aoc.y2019
import scala.collection.immutable.Queue

object Day09 extends Puzzle2019[Vector[Long], Long, Long] {
  override val input: Vector[Long] = getInputFile.head.split(",").toVector.map(_.toLong)

  trait ParamMode
  object Position extends ParamMode
  object Immediate extends ParamMode
  object Relative extends ParamMode

  trait Mode
  object Running extends Mode
  object Halt extends Mode
  object Terminated extends Mode

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

  private def readFromPosition(program: Vector[Long], pos: Int) =
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
        if (state.inputs.isEmpty) (Halt, State(state.program, state.index, state.inputs, state.outputs))
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
      case 99 => (Terminated, State(state.program, state.index, state.inputs, state.outputs))
    }
  }

  override def resolveFirst(input: Vector[Long]): Long = execute(State(program = input, inputs = Queue(1)))._2.outputs.mkString.toLong

  override def resolveSecond(input: Vector[Long]): Long = execute(State(program = input, inputs = Queue(2)))._2.outputs.mkString.toLong
}
