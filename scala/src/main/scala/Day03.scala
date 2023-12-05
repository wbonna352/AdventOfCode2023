import scala.io.Source

object Day03 {

  def part1(): Int = {
    val input: Array[Array[Char]] = Source.fromFile("inputs/Day03.input")
      .getLines()
      .map(_.toCharArray)
      .toArray

    val symbols: Array[Char] =
      """~!@#$^%&*()_-+={}[]|;:\"'<,>?/"""
        .toCharArray

    val lineLength: Int = input(0).length
    val inputFlat: Array[Char] = input.flatten
    val inputSize = inputFlat.length

    val symbolsPositions: Array[Int] = inputFlat
      .zipWithIndex
      .filter(pair => symbols.contains(pair._1))
      .map(_._2)

    val adjacentPositions: Array[Int] =
      symbolsPositions
        .flatMap { i =>
          (0 until inputSize)
            .filter(p =>
              (((p / lineLength) - (i / lineLength)).abs <= 1)
                && (((p % lineLength) - (i % lineLength)).abs <= 1)
            )
        }

    "\\d+".r
      .findAllMatchIn(inputFlat.mkString)
      .map(m => ((m.start until m.end).toArray, m.matched.toInt))
      .filter(_._1.intersect(adjacentPositions).nonEmpty)
      .map(_._2)
      .sum

  }

  def part2(): Int = {
    val symbol: Char = '*'

    val input: Array[Array[Char]] = Source.fromFile("inputs/Day03.input")
      .getLines()
      .map(_.toCharArray)
      .toArray

    val lineLength: Int = input(0).length
    val inputFlat: Array[Char] = input.flatten
    val inputSize = inputFlat.length

    val symbolPositions: Array[Int] = inputFlat
      .zipWithIndex
      .filter(_._1 == symbol)
      .map(_._2)

    val adjacentPositions: Array[(Int, Array[Int])] =
      symbolPositions
        .map { i =>
          (i,
          (0 until inputSize)
            .filter(p =>
              (((p / lineLength) - (i / lineLength)).abs <= 1)
                && (((p % lineLength) - (i % lineLength)).abs <= 1)
            ).toArray)
        }

    val values: Array[(Array[Int], Int)] =
      "\\d+".r
        .findAllMatchIn(inputFlat.mkString)
        .map(m => ((m.start until m.end).toArray, m.matched.toInt))
        .toArray

    adjacentPositions
      .map(symbol =>
        (symbol, values.filter(_._1.intersect(symbol._2).nonEmpty)))
      .filter(_._2.length == 2)
      .map(symbol => symbol._2.map(_._2))
      .map(_.product)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1())
    println("Part2: " + part2())

  }
}
