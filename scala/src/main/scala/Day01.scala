import scala.io.Source
import scala.util.Using

object Day01 {

  def calibrationValue(input: String): Int = {
    val first: Char = input.find(_.isDigit).get
    val last: Char = input.findLast(_.isDigit).get
    List(first, last).mkString.toInt
  }

  def dayOne(): Unit = {
    val input: Iterator[String] = Source.fromFile("inputs/Day01.input").getLines()
    val result: Int =
      input
      .map(calibrationValue)
      .sum

    println(result)
  }

  def main(args: Array[String]): Unit = {
    dayOne()
  }


}
