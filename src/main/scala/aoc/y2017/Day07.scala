package aoc.y2017

import scala.util.parsing.combinator._

object Day07 extends Puzzle2017[List[String], String, Int] with RegexParsers {
  override val input: List[String] = getInputFile

  case class SimpleNode(name: String, value: Int, children: Vector[String]) {
    def getChildren(nodes: Map[String, SimpleNode]): Vector[SimpleNode] = children.map(nodes)

    def toNode(nodes: Map[String, SimpleNode]): Node = Node(
      name,
      value,
      getChildren(nodes).map(_.toNode(nodes))
    )
  }

  case class Node(name: String, weight: Int, children: Vector[Node]) {
    def getWeightProgram: Int = children match {
      case Vector() => weight
      case nodes => weight + nodes.map(_.getWeightProgram).sum
    }
  }

  val nodeParser: Parser[SimpleNode] = for {
    name <- """[a-z]+""".r
    weight <- ("(" ~> """\d+""".r <~ ")") ^^ (_.toInt)
    children <- ("->" ~> repsep("""[a-z]+""".r, ", ")).? ^^ (_.map(_.toVector))
  } yield SimpleNode(name, weight, children.getOrElse(Vector.empty))

  def parseTree(input: List[String]): Map[String, SimpleNode] = {
    input
      .map(node => parse(nodeParser, node).get)
      .map(node => Map(node.name -> node))
      .reduce(_ ++ _)
  }

  def getRoot(nodes: Map[String, SimpleNode]): SimpleNode = {
    val nodeNames = nodes.keys.toVector
    val notRootNodeNames = nodes.values.toVector.flatMap(_.children).distinct
    val rootNodeName = (nodeNames diff notRootNodeNames).head
    nodes(rootNodeName)
  }

  override def part1(input: List[String]): String = getRoot(parseTree(input)).name

  override def part2(input: List[String]): Int = {
    val nodes = parseTree(input)
    val root = getRoot(nodes).toNode(nodes)
    def rec(node: Node): (Boolean, Int) = node.children match {
      case Vector() => (true, node.weight)
      case children => children.map(rec).find(value => !value._1) match {
        case None =>
          val groupedPrograms = children.groupBy(_.getWeightProgram)
          if (groupedPrograms.size == 1) (true, node.getWeightProgram)
          else {
            val (rightPrograms, wrongProgram) = groupedPrograms.partition(_._2.size != 1)
            val (rightWeight, wrongWeight) = (rightPrograms.head._1, wrongProgram.head._1)
            val wrongNode = wrongProgram.head._2.head
            (false, wrongNode.weight - (wrongWeight - rightWeight))
          }
        case Some(balanced) => balanced
      }
    }
    rec(root)._2
  }
}
