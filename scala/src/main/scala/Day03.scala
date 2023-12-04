import scala.io.Source

object Day03 {

  val symbols: Array[Char] =
    """~!@#$^%&*()_-+={}[]|;:\"'<,>?/"""
      .toCharArray

  def part1(): Int = {
    val input: Array[Array[Char]] = Source.fromFile("inputs/Day03.input")
      .getLines()
      .map(_.toCharArray)
      .toArray

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

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1())
  }
}
