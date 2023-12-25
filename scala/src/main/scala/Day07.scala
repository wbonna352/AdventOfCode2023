import scala.io.Source

object Day07 {

  object HandType extends Enumeration {
    type HandType = Value
    val High, Pair, TwoPair, Three, FullHouse, Four, Five = Value
  }

  type Hand = Array[Card.Value]

  object Card extends Enumeration {
    type Card = Value
    val Two, Three, Four, Five, Six, Seven, Eight, Nine, T, J, Q, K, A = Value
  }

  def charToCard(char: Char): Card.Value = {
    char match {
      case '2' => Card.Two
      case '3' => Card.Three
      case '4' => Card.Four
      case '5' => Card.Five
      case '6' => Card.Six
      case '7' => Card.Seven
      case '8' => Card.Eight
      case '9' => Card.Nine
      case 'T' => Card.T
      case 'J' => Card.J
      case 'Q' => Card.Q
      case 'K' => Card.K
      case 'A' => Card.A
    }
  }

  def handToType(hand: Hand): HandType.Value = {
    val counter: Map[Card.Value, Int] = hand.groupBy(identity).view.mapValues(_.length).toMap

    counter match {
      case x if x.values.max == 5 => HandType.Five
      case x if x.values.max == 4 => HandType.Four
      case x if x.values.toList.sorted == List(2, 3) => HandType.FullHouse
      case x if x.values.max == 3 => HandType.Three
      case x if x.values.toList.sorted == List(1, 2, 2) => HandType.TwoPair
      case x if x.values.max == 2 => HandType.Pair
      case x if x.values.max == 1 => HandType.High
    }
  }

  def parse(input: Iterator[String]): List[(String, Int)] = {
    input
      .map {
        case s"$hand $bid" => (hand, bid.toInt)
      }
      .toList
  }

  val handOrdering: Ordering[Hand] = (x: Hand, y: Hand) => {
    if (handToType(x).id == handToType(y).id) {
      (x zip y).find(pair => pair._1 != pair._2) match {
        case Some((a, b)) => a.id - b.id
        case None => 0
      }
    }
    else handToType(x).id - handToType(y).id
  }

  val jokerOrdering: Ordering[Hand] = (x: Hand, y: Hand) => {

    val xType: HandType.Value = handToType(jokerBestReplace(x))
    val yType: HandType.Value = handToType(jokerBestReplace(y))

    if (xType.id == yType.id) {
      (x zip y).find(pair => pair._1 != pair._2) match {
        case Some((a, b)) =>
          if (a == Card.J) -1
          else if (b == Card.J) 1
          else a.id - b.id
        case None => 0
      }
    }
    else xType.id - yType.id
  }

  def jokerBestReplace(hand: Hand): Hand = {
    if (hand.count(_ == Card.J) == 5) hand
    else {
      val mode: Card.Value = {
        hand.filter(_ != Card.J).groupBy(identity).view.mapValues(_.length).maxBy(_._2)._1
      }

      hand.map {
        case Card.J => mode
        case c => c
      }
    }
  }

  def part1(): Int = {
    val input = Source.fromFile("inputs/Day07.input").getLines()

    parse(input)
      .map(l => (l._1.toCharArray.map(charToCard), l._2))
      .sortBy(_._1)(handOrdering)
      .zipWithIndex
      .map(h => h._1._2 * (h._2 + 1))
      .sum
  }

  def part2(): Int = {
    val input = Source.fromFile("inputs/Day07.input").getLines()

    parse(input)
      .map(l => (l._1.toCharArray.map(charToCard), l._2))
      .sortBy(_._1)(jokerOrdering)
      .zipWithIndex
      .map(h => h._1._2 * (h._2 + 1))
      .sum

  }

  def main(args: Array[String]): Unit = {
    println("Part1: " + part1())
    println("Part2: " + part2())
  }

}
