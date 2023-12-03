import scala.io.Source
import scala.util.matching.Regex

object Day01 {

  def calibrationValue(input: String): Int = {
    val first: Char = input.find(_.isDigit).get
    val last: Char = input.findLast(_.isDigit).get
    List(first, last).mkString.toInt
  }

  def calibrationValueWithWords(input: String): Int = {

    val digits: Map[String, Int] = (1 to 9).map(n => (n.toString, n)).toMap
    val words: Map[String, Int] = Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9
    )

    val mapper: Map[String, Int] = digits ++ words

    val matchPattern: Regex =
      mapper
      .keys
      .mkString("|")
      .r

    val matches: Iterator[String] =
      input
        .tails
        .map(matchPattern.findPrefixOf)
        .filter(_.nonEmpty)
        .map(_.get)

    val first = matches.next()
    var last = first
    while (matches.hasNext) last = matches.next()

    s"${mapper(first)}${mapper(last)}".toInt

  }

  def partOne(): Int = {
    val input: Iterator[String] = Source.fromFile("inputs/Day01.input").getLines()

      input
        .map(calibrationValue)
        .sum
  }

  def partTwo(): Int = {
    val input: Iterator[String] = Source.fromFile("inputs/Day01.input").getLines()

      input
        .map(calibrationValueWithWords)
        .sum
  }


  def main(args: Array[String]): Unit = {
    println("Part one: " + partOne())
    println("Part two: " + partTwo())
  }

}
