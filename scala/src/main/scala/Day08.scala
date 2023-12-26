import scala.annotation.tailrec
import scala.io.Source

object Day08 {

  type Node = String

  case class NetworkLine(main: Node, left: Node, right: Node)

  def stringToNetworkLine(str: String): NetworkLine = {
    "(\\w+) = \\((\\w+), (\\w+)\\)".r
      .findAllIn(str)
      .subgroups match {
      case List(m, l, r) => NetworkLine(m, l, r)
    }
  }

  def findNextNode(current: Node, instruction: Char, network: List[NetworkLine]): Node = {
    val line: NetworkLine = network.find(_.main == current).get
    instruction match {
      case 'L' => line.left
      case 'R' => line.right
    }
  }

  def execute(startNode: Node, finalNode: Node, instructions: Array[Char], network: List[NetworkLine]): Int = {

    @tailrec
    def run(n: Int, currentNode: Node, currentInstructions: Array[Char]): Int = {
      if (currentNode == finalNode) n
      else run(
        n+1,
        findNextNode(currentNode, currentInstructions.head, network),
        currentInstructions.tail :+ currentInstructions.head
      )
    }
    run(0, startNode, instructions)
  }

  def part1(input: List[String]): Int = {
    val startNode: Node = "AAA"
    val finalNode: Node = "ZZZ"
    val instructions: Array[Char] = input.head.toCharArray
    val network: List[NetworkLine] = input
      .drop(2)
      .map(stringToNetworkLine)

    execute(startNode, finalNode, instructions, network)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("inputs/Day08.input").getLines().toList
    println("Part1: " + part1(input))
  }

}
