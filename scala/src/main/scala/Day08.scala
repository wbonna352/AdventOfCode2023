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

  def execute(startNode: Node, pred: Node => Boolean, instructions: Array[Char], network: List[NetworkLine]): Int = {

    @tailrec
    def run(n: Int, node: Node, currentInstructions: Array[Char]): Int = {
      if (pred(node)) n
      else run(
        n+1,
        findNextNode(node, currentInstructions.head, network),
        currentInstructions.tail :+ currentInstructions.head
      )
    }
    run(0, startNode, instructions)
  }

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  @tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) a
    else gcd(b, a % b)

  def multipleExecute(startNodes: List[Node], pred: Node => Boolean, instructions: Array[Char], network: List[NetworkLine]): Long = {

    startNodes
      .map(execute(_, pred, instructions, network).toLong)
      .reduce(lcm)
  }

  def part1(instructions: Array[Char], network: List[NetworkLine]): Int = {
    val startNode: Node = "AAA"
    def pred(node: Node): Boolean = node == "ZZZ"

    execute(startNode, pred, instructions, network)
  }

  def part2(instructions: Array[Char], network: List[NetworkLine]): Long = {
    val startNodes: List[Node] = network
      .filter(_.main.endsWith("A"))
      .map(_.main)
    def endCondition(node: Node): Boolean = node.endsWith("Z")

    multipleExecute(startNodes, endCondition, instructions, network)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("inputs/Day08.input").getLines().toList
    val instructions: Array[Char] = input.head.toCharArray
    val network: List[NetworkLine] = input
      .drop(2)
      .map(stringToNetworkLine)

    println("Part1: " + part1(instructions, network))
    println("Part2: " + part2(instructions, network))
  }

}
