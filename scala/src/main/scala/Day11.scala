import scala.io.Source

object Day11 {

  def getDistance(galaxyA: (Int, Int), galaxyB: (Int, Int), emptyRows: Array[Int], emptyCols: Array[Int]): Int = {
    val rangeX: Range =
      if (galaxyA._1 > galaxyB._1) galaxyB._1 until galaxyA._1
      else galaxyA._1 until galaxyB._1

    val rangeY: Range =
      if (galaxyA._2 > galaxyB._2) galaxyB._2 until galaxyA._2
      else galaxyA._2 until galaxyB._2

    val addedRows: Array[Int] = rangeX.toArray.intersect(emptyRows)
    val addedCols: Array[Int] = rangeY.toArray.intersect(emptyCols)

    rangeX.length + rangeY.length + addedRows.length + addedCols.length
  }

  def getDistance2(galaxyA: (Int, Int), galaxyB: (Int, Int), emptyRows: Array[Int], emptyCols: Array[Int]): Long = {
    val rangeX: Range =
      if (galaxyA._1 > galaxyB._1) galaxyB._1 until galaxyA._1
      else galaxyA._1 until galaxyB._1

    val rangeY: Range =
      if (galaxyA._2 > galaxyB._2) galaxyB._2 until galaxyA._2
      else galaxyA._2 until galaxyB._2

    val addedRows: Array[Int] = rangeX.toArray.intersect(emptyRows)
    val addedCols: Array[Int] = rangeY.toArray.intersect(emptyCols)

    (
      rangeX.length.toLong
        + rangeY.length.toLong
        + addedRows.length.toLong * (1000000 - 1)
        + addedCols.length.toLong * (1000000 - 1)
      )
  }

  val input = Source.fromFile("inputs/Day11.input").getLines()

  val arr: Array[Array[Char]] = input.map(_.toCharArray).toArray

  val galaxies: Array[(Int, Int)] =
    arr
    .map(_.zipWithIndex.filter(_._1 == '#').map(_._2))
    .zipWithIndex
    .flatMap(row => row._1.map(( row._2, _)))

  val emptyRows: Array[Int] =
    arr
      .zipWithIndex
      .filter(r => r._1.distinct.length == 1)
      .map(_._2)

  val emptyCols: Array[Int] =
    arr
      .transpose
      .zipWithIndex
      .filter(r => r._1.distinct.length == 1)
      .map(_._2)


  def part1(): Int = {
    galaxies
      .combinations(2)
      .map(comb => getDistance(comb.head, comb.last, emptyRows, emptyCols))
      .sum
  }

  def part2(): Long = {
    galaxies
      .combinations(2)
      .map(comb => getDistance2(comb.head, comb.last, emptyRows, emptyCols))
      .sum
  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1())
    println("Part2: " + part2())

  }

}
