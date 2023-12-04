import scala.io.Source

object Day02 {

  case class Set(blue: Int, green: Int, red: Int) {
    def isSubsetOf(set: Set): Boolean = {
      this.blue <= set.blue &&
        this.red <= set.red &&
        this.green <= set.green
    }
  }
  object Set {
    def apply(line: String): Set = {
      val args: Map[String, Int] = line
        .split(",")
        .map {
          case s" $value $color" => color -> value.toInt
        }
        .toMap

      Set(
        blue=args.getOrElse("blue", 0),
        green=args.getOrElse("green", 0),
        red=args.getOrElse("red", 0)
      )
    }
  }

  case class Game(id: Int, sets: Seq[Set]) {
    def bagCheck(bag: Set): Boolean = {
      this.sets
        .map(_.isSubsetOf(bag))
        .forall(_ == true)
    }

    def smallestBag: Set = Set(
        blue=sets.map(_.blue).max,
        red=sets.map(_.red).max,
        green=sets.map(_.green).max
      )

  }
  object Game {
    def apply(line: String): Game = {

      val Array(idInput, setsInput) = line.split(":", 2)
      val id: Int = idInput match {
        case s"Game $id" => id.toInt
      }
      val sets: Seq[Set] =
        setsInput
          .split(";")
          .map(Set(_))
          .toSeq

      Game(id, sets)
    }
  }

  def part1(): Int = {
    val input = Source.fromFile("inputs/Day02.input").getLines()

    val bag: Set = Set(red=12, green=13, blue=14)

    input
      .map(Game(_))
      .filter(_.bagCheck(bag))
      .map(_.id)
      .sum
  }

  def part2(): Int = {
    val input = Source.fromFile("inputs/Day02.input").getLines()

    input
      .map(Game(_))
      .map(_.smallestBag)
      .map(s => s.blue * s.red * s.green)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println("Part one: " + part1())
    println("Part two: " + part2())
  }
}
