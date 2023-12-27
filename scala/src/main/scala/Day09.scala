import scala.annotation.tailrec
import scala.io.Source

object Day09 {

  def calculateRows(row: Array[Int]): Array[Array[Int]] = {

    @tailrec
    def run(rows: Array[Array[Int]]): Array[Array[Int]] = {
      if ((rows.last.length == 1) || !rows.last.exists(_ != 0)) rows
      else {
        val nextRow: Array[Int] =
          rows.last
            .sliding(2)
            .map(pair => pair.last - pair.head)
            .toArray

        run(rows :+ nextRow)
      }
    }
    run(Array(row))
  }


  def calculateFromRight(input: Array[Int]): Int = {
    calculateRows(input)
      .foldRight(0)((row: Array[Int], currVal: Int) => row.last + currVal)
  }

  def calculateFromLeft(input: Array[Int]): Int = {
    calculateRows(input)
      .foldRight(0)((row: Array[Int], currVal: Int) => row.head - currVal)
  }

  def part1: Int = {
    val input: Iterator[String] = Source.fromFile("inputs/Day09.input").getLines()

    input
      .map(_.split(" ").map(_.toInt))
      .map(calculateFromRight)
      .sum
  }

  def part2: Int = {
    val input: Iterator[String] = Source.fromFile("inputs/Day09.input").getLines()

    input
      .map(_.split(" ").map(_.toInt))
      .map(calculateFromLeft)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1)
    println("Part2: " + part2)

  }
}
