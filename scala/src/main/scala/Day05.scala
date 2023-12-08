import scala.io.Source

object Day05 {

  case class Mapper(source: String, destination: String, mapValues: Array[Array[BigInt]]) {
    def get(value: BigInt): BigInt = {
      mapValues
        .map(l =>
          (
            l(1) until (l(1) + l(2)) contains value,
            value - l(1) + l(0)
          )
        )
        .find(_._1)
        .map(_._2)
        .getOrElse(value)

    }
  }
  object Mapper {
    def apply(input: String): Mapper = {

      val lines: Array[String] = input.split("\n")
      val List(source, destination): List[String] =
        """^(\w+)-to-(\w+)\smap:$""".r
          .findFirstMatchIn(lines.head)
          .get
          .subgroups

      val mapValues: Array[Array[BigInt]] = lines
        .tail
        .map(_.split(" ").map(BigInt(_)))


      Mapper(source, destination, mapValues)
      }

    }


  def part1(): BigInt = {
    val input = Source.fromFile("inputs/Day05.input").mkString.split("\n\n")

    val seeds: List[BigInt] =
      "\\d+".r
        .findAllIn(input.head)
        .map(BigInt(_))
        .toList

    val mappers =
      input
        .tail
        .map(Mapper(_))

    mappers
      .foldLeft(seeds)(
        (currVal: List[BigInt], m: Mapper) =>
          currVal.map(v => m.get(v)))
      .min
  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1())
  }
}
