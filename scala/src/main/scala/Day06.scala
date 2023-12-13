import scala.collection.immutable.{NumericRange, Seq}
import scala.io.Source
import scala.math.{pow, sqrt}

object Day06 {

  case class Record(time: Long, distance: Long) {
    def X: NumericRange.Inclusive[Long] = {
      val delta: Double = pow(time, 2) - 4 * distance
      val x1: Double = (time - sqrt(delta)) / 2
      val x2: Double = (time + sqrt(delta)) / 2
      x1.ceil.toLong to x2.floor.toLong
    }
  }

  def parse(input: String): Seq[Record] = {
    val times: Seq[Long] =
      input
        .split("\n")
        .head
        .split("\\s+")
        .tail
        .map(_.toLong)

    val distances: Seq[Long] =
      input
        .split("\n")
        .last
        .split("\\s+")
        .tail
        .map(_.toLong)

    times
      .zip(distances)
      .map(r => Record(r._1, r._2))
  }

  def parse2(input: String): Record = {
    val time: Long =
      input
        .split("\n")
        .head
        .filter(_.isDigit)
        .toLong

    val distance: Long =
      input
        .split("\n")
        .last
        .filter(_.isDigit)
        .toLong

    Record(time, distance)
  }

  def part1(): Long = {
    val input = Source.fromFile("inputs/Day06.input").mkString

    parse(input)
      .map(_.X.length)
      .product
  }

  def part2(): Long = {
    val input = Source.fromFile("inputs/Day06.input").mkString
    parse2(input).X.length
  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1())
    println("Part2: " + part2())
  }
}
