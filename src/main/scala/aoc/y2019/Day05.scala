package aoc.y2019
import scala.collection.immutable.Queue

object Day05 extends Puzzle2019[Vector[Int], Int, Int] {
  override val input: Vector[Int] = getInputFile.head.split(",").map(_.toInt).toVector

  trait ParamMode
  object Position extends ParamMode
  object Immediate extends ParamMode

  case class Instruction(instruction: Int, param1: ParamMode, param2: ParamMode, param3: ParamMode)

  private def parseInstruction(value: Int) =
    if (value <= 99) Instruction(value, Position, Position, Position)
    else Instruction(
      value % 100,
      if (value/100 % 10 == 0) Position else Immediate,
      if (value/1000 % 10 == 0) Position else Immediate,
      if (value/10000 % 10 == 0) Position else Immediate
    )

  private def getParam(mode: ParamMode, value: Int, input: Vector[Int]): Int = mode match {
    case Position => input(input(value))
    case Immediate => input(value)
  }

  private def execute(program: Vector[Int], index: Int = 0, inputs: Queue[Int] = Queue(1), outputs: Queue[Int] = Queue.empty): Int = parseInstruction(program(index)) match {
    case Instruction(instruction, param1, param2, param3) => instruction match {
      case 1 =>
        execute(program.updated(getParam(Immediate, index + 3, program), getParam(param1, index + 1, program) + getParam(param2, index + 2, program)), index + 4, inputs, outputs)
      case 2 =>
        execute(program.updated(getParam(Immediate, index + 3, program), getParam(param1, index + 1, program) * getParam(param2, index + 2, program)), index + 4, inputs, outputs)
      case 3 =>
        val (first, newInputs) = inputs.dequeue
        execute(program.updated(getParam(Immediate, index + 1, program), first), index + 2, newInputs, outputs)
      case 4 =>
        val newOutputs = outputs.enqueue(getParam(param1, index + 1, program))
        execute(program, index + 2, inputs, newOutputs)
      case 5 =>
        getParam(param1, index + 1, program) match {
          case 0 => execute(program, index + 3, inputs, outputs)
          case _ => execute(program, getParam(param2, index + 2, program), inputs, outputs)
        }
      case 6 =>
        getParam(param1, index + 1, program) match {
          case 0 => execute(program, getParam(param2, index + 2, program), inputs, outputs)
          case _ => execute(program, index + 3, inputs, outputs)
        }
      case 7 =>
        (getParam(param1, index + 1, program), getParam(param2, index + 2, program)) match {
          case (left, right) if left < right =>
            execute(program.updated(getParam(Immediate, index + 3, program), 1), index + 4, inputs, outputs)
          case _ =>
            execute(program.updated(getParam(Immediate, index + 3, program), 0), index + 4, inputs, outputs)
        }
      case 8 =>
        (getParam(param1, index + 1, program), getParam(param2, index + 2, program)) match {
          case (left, right) if left == right =>
            execute(program.updated(getParam(Immediate, index + 3, program), 1), index + 4, inputs, outputs)
          case _ =>
            execute(program.updated(getParam(Immediate, index + 3, program), 0), index + 4, inputs, outputs)
        }
      case 99 => outputs.mkString.toInt
    }
  }

  override def part1(input: Vector[Int]): Int = execute(input)

  override def part2(input: Vector[Int]): Int = execute(input, 0, Queue(5))
}