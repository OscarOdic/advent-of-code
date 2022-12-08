package aoc.y2019
import scala.collection.immutable.Queue

object Day07 extends Puzzle2019[Vector[Int], Int, Int] {
  override val input: Vector[Int] = getInputFile.head.split(",").toVector.map(_.toInt)

  trait ParamMode
  object Position extends ParamMode
  object Immediate extends ParamMode

  trait Mode
  object Running extends Mode
  object Halt extends Mode
  object Terminated extends Mode

  case class State(program: Vector[Int], index: Int = 0, inputs: Queue[Int], outputs: Queue[Int] = Queue.empty)

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

  private def execute(state: State): (Mode, State) = parseInstruction(state.program(state.index)) match {
    case Instruction(instruction, param1, param2, param3) => instruction match {
      case 1 =>
        execute(state.copy(program = state.program.updated(getParam(Immediate, state.index + 3, state.program), getParam(param1, state.index + 1, state.program) + getParam(param2, state.index + 2, state.program)), index = state.index + 4))
      case 2 =>
        execute(state.copy(program = state.program.updated(getParam(Immediate, state.index + 3, state.program), getParam(param1, state.index + 1, state.program) * getParam(param2, state.index + 2, state.program)), index = state.index + 4))
      case 3 =>
        if (state.inputs.isEmpty) (Halt, State(state.program, state.index, state.inputs, state.outputs))
        else {
          val (first, newInputs) = state.inputs.dequeue
          execute(state.copy(program = state.program.updated(getParam(Immediate, state.index + 1, state.program), first), index = state.index + 2, inputs = newInputs))
        }
      case 4 =>
        val newOutputs = state.outputs.enqueue(getParam(param1, state.index + 1, state.program))
        execute(state.copy(index = state.index + 2, outputs = newOutputs))
      case 5 =>
        getParam(param1, state.index + 1, state.program) match {
          case 0 => execute(state.copy(program = state.program, index = state.index + 3))
          case _ => execute(state.copy(program = state.program, index = getParam(param2, state.index + 2, state.program)))
        }
      case 6 =>
        getParam(param1, state.index + 1, state.program) match {
          case 0 => execute(state.copy(program = state.program, index = getParam(param2, state.index + 2, state.program)))
          case _ => execute(state.copy(program = state.program, index = state.index + 3))
        }
      case 7 =>
        (getParam(param1, state.index + 1, state.program), getParam(param2, state.index + 2, state.program)) match {
          case (left, right) if left < right =>
            execute(state.copy(program = state.program.updated(getParam(Immediate, state.index + 3, state.program), 1), index = state.index + 4))
          case _ =>
            execute(state.copy(program = state.program.updated(getParam(Immediate, state.index + 3, state.program), 0), index = state.index + 4))
        }
      case 8 =>
        (getParam(param1, state.index + 1, state.program), getParam(param2, state.index + 2, state.program)) match {
          case (left, right) if left == right =>
            execute(state.copy(program = state.program.updated(getParam(Immediate, state.index + 3, state.program), 1), index = state.index + 4))
          case _ =>
            execute(state.copy(program = state.program.updated(getParam(Immediate, state.index + 3, state.program), 0), index = state.index + 4))
        }
      case 99 => (Terminated, State(state.program, state.index, state.inputs, state.outputs))
    }
  }

  def feedbackLoop(states: Vector[(Mode, State)], indexAmplifier: Int = 0): Int = {
    val (mode, state) = states(indexAmplifier)
    val (indexPrevious, indexNext) = ((indexAmplifier + states.size - 1) % states.size, (indexAmplifier + 1) % states.size)
    val isLast = indexAmplifier == states.size - 1

    val previous = states(indexPrevious)
    if(mode == Terminated)
      if(isLast) state.outputs.head
      else feedbackLoop(states, indexNext)
    else {
      val next = execute(state.copy(inputs = state.inputs ++ previous._2.outputs))
      val nextStates = states.updated(indexPrevious, previous.copy(_2 = previous._2.copy(outputs = Queue.empty))).updated(indexAmplifier, next)
      feedbackLoop(nextStates, indexNext)
    }
  }

  override def part1(input: Vector[Int]): Int =
    (0 to 4).permutations.map(
      seq => seq.foldLeft(0)((output, i) => execute(State(input, 0, Queue(i, output), Queue.empty))._2.outputs.mkString.toInt)
    ).max

  override def part2(input: Vector[Int]): Int = {
    (5 to 9).permutations.map(amp =>
      feedbackLoop((Queue(amp.head, 0) +: amp.tail.map(v => Queue(v))).map(in => (Running, State(program = input, inputs = in))).toVector)
    ).max
  }
}
