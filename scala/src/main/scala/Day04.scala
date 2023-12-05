import scala.io.Source
import scala.util.matching.Regex
import scala.math.pow

object Day04 {

  case class Card(id: Int, win: Array[Int], played: Array[Int]) {
    def points: Int = {
      val cards: Int = win.intersect(played).length
      pow(2, cards - 1).toInt
    }
  }
  object Card {
    def apply(line: String): Card = {
      val pattern: Regex = """^Card\s+(\d+): ([\d\s]+)\|([\d\s]+)$""".r
      pattern
        .findAllIn(line)
        .subgroups match {
        case List(i, w, p) =>
          val id: Int = i.toInt
          val win: Array[Int] = w.strip().split("\\s+").map(_.toInt)
          val played: Array[Int] = p.strip().split("\\s+").map(_.toInt)
          Card(id, win, played)
        }
    }
  }

  def part1(): Int = {
    val input = Source.fromFile("inputs/Day04.input").getLines()

    input
      .map(Card(_).points)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1())
  }
}
