package adventOfCode

object Day3 {

  trait Direction
  object Up extends Direction
  object Down extends Direction
  object Right extends Direction
  object Left extends Direction

  val input = 325489

  def getSide(n: Int) = {
    val side = math.ceil(math.sqrt(n)).toInt
    if (side % 2 != 0) side
    else side + 1
  }

  def getDirection(initialPosition: (Int, Int), defaultDirection: Direction) = initialPosition match {
    case (0, 0) => Right
    case (l, r) if l == r && l > 0 => Left
    case (l, r) if l == r && l < 0 => Right
    case (l, r) if l - 1 == -r && l > 0 => Up
    case (l, r) if l == -r && l < 0 => Down
    case _ => defaultDirection
  }

  def getNeighbors(position: (Int, Int)) =
    List(
      (position._1, position._2 - 1),
      (position._1, position._2 + 1),
      (position._1 - 1, position._2),
      (position._1 - 1, position._2 - 1),
      (position._1 - 1, position._2 + 1),
      (position._1 + 1, position._2),
      (position._1 + 1, position._2 - 1),
      (position._1 + 1, position._2 + 1)
    )

  def getValue(grid: Map[(Int, Int), Int], position: (Int, Int)) =
    getNeighbors(position).flatMap(grid.get).sum


  def resolveFirst(n: Int) = {
    val side = getSide(n)
    val steps = (side-1)/2
    val cycle = n - math.pow(side - 2, 2).toInt
    val innerOffset = cycle % (side - 1)

    steps + math.abs(innerOffset - steps)
  }

  def resolveSecond(n: Int) = {
    def rec(grid: Map[(Int, Int), Int], position: (Int, Int), n: Int, direction: Direction): Int = {
      val newDirection = getDirection(position, direction)
      val newPosition = newDirection match {
        case Up => (position._1, position._2 + 1)
        case Down => (position._1, position._2 - 1)
        case Left => (position._1 - 1, position._2)
        case Right => (position._1 + 1, position._2)
      }
      val value = getValue(grid, newPosition)
      if (value > n) value
      else rec(grid.updated(newPosition, value), newPosition, n, newDirection)
    }

    rec(Map((0, 0) -> 1, (1, 0) -> 1), (1, 0), n, Up)
  }

  def main(args: Array[String]): Unit = {
    println(s"[first star] ${resolveFirst(input)}")
    println(s"[second star] ${resolveSecond(input)}")
  }
}
