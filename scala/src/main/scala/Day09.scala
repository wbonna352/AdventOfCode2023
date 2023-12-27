import scala.annotation.tailrec
import scala.io.Source

object Day09 {

  def calculate(input: Array[Int]): Int = {
    if (input.length == 1) input.head
    else {
      input
        .sliding(2)
        .map(pair => pair.last - pair.head)
        .toArray
    }

    @tailrec
    def calculateRows(rows: Array[Array[Int]]): Array[Array[Int]] = {
      if ((rows.last.length == 1) || !rows.last.exists(_ != 0)) rows
      else {
        val nextRow: Array[Int] =
          rows.last
            .sliding(2)
            .map(pair => pair.last - pair.head)
            .toArray

        calculateRows(rows :+ nextRow)
      }
    }

    val rows: Array[Array[Int]] = calculateRows(Array(input))

    rows
      .foldRight(0)((row: Array[Int], currVal: Int) => row.last + currVal)
  }

  def part1: Int = {
    val input: Iterator[String] = Source.fromFile("inputs/Day09.input").getLines()

    input
      .map(_.split(" ").map(_.toInt))
      .map(calculate)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1)

  }
}
