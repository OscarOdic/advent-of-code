package aoc.y2017

object Day22 extends Puzzle2017[Map[(Int, Int), Boolean], Int, Int] {
  override val input: Map[(Int, Int), Boolean] = toBooleanGrid(getInputFile)

  def toBooleanGrid(input: List[String]): Map[(Int, Int), Boolean] = (
    for {
      y <- input.indices
      x <- input(y).indices
    } yield ((x, y), getInputFile(y)(x) == '#')
    ).toMap

  trait Direction { def next(state: State): Direction }
  object Up extends Direction {
    def next(state: State): Direction = state match {
      case Clean => Left
      case Weakened => Up
      case Infected => Right
      case Flagged => Down
    }
  }
  object Down extends Direction {
    def next(state: State): Direction = state match {
      case Clean => Right
      case Weakened => Down
      case Infected => Left
      case Flagged => Up
    }
  }
  object Right extends Direction {
    def next(state: State): Direction = state match {
      case Clean => Up
      case Weakened => Right
      case Infected => Down
      case Flagged => Left
    }
  }
  object Left extends Direction {
    def next(state: State): Direction = state match {
      case Clean => Down
      case Weakened => Left
      case Infected => Up
      case Flagged => Right
    }
  }

  trait State { def next: State }
  object Clean extends State { def next = Weakened }
  object Weakened extends State { def next = Infected }
  object Infected extends State { def next = Flagged }
  object Flagged extends State { def next = Clean }

  private def move(position: (Int, Int), direction: Direction) = direction match {
    case Up => (position._1, position._2 - 1)
    case Left => (position._1 - 1, position._2)
    case Down => (position._1, position._2 + 1)
    case Right => (position._1 + 1, position._2)
  }

  private def execute(grid: Map[(Int, Int), State], position: (Int, Int), direction: Direction, secondPuzzle: Boolean) = {
    val value = grid.getOrElse(position, Clean)
    val newDirection = direction.next(value)
    val newState =
      if (secondPuzzle) value.next
      else if (value == Clean) Infected
      else Clean
    (
      grid.updated(position, newState),
      move(position, newDirection),
      newDirection,
      newState == Infected
    )
  }

  private def getCenter[A](grid: Map[(Int, Int), A]) = {
    val (maxX, maxY) = grid.keys.reduce((a,b) => (math.max(a._1,b._1), math.max(a._2, b._2)))
    (math.ceil(maxX/2.0).toInt, math.ceil(maxY/2.0).toInt)
  }

  private def toStateMap(grid: Map[(Int, Int), Boolean]) =
    grid.view.mapValues(value => if (value) Infected else Clean).toMap

  def resolve(input: Map[(Int, Int), Boolean], nbBursts: Int = 10000, secondPuzzle: Boolean = false): Int = {
    (0 until nbBursts).foldLeft(((toStateMap(input), getCenter(input), Up: Direction), 0)) {
      case (((grid, position, direction), nbInfection), _) =>
        val (newGrid, newPosition, newDirection, infected) = execute(grid, position, direction, secondPuzzle)
        ((newGrid, newPosition, newDirection), if (infected) nbInfection + 1 else nbInfection)
    }._2
  }

  override def part1(input: Map[(Int, Int), Boolean]): Int = resolve(input)

  override def part2(input: Map[(Int, Int), Boolean]): Int = resolve(input, 10000000, secondPuzzle = true)
}
