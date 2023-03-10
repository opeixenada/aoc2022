import Util.readFile

import scala.annotation.tailrec

@main def day25(): Unit = {

  def toDecimal(snafu: String): BigInt = snafu.reverse.zipWithIndex.map { (ch, i) =>
    val n = ch match
      case '-' => -1
      case '=' => -2
      case x => x.toInt - '0'

    BigInt(5).pow(i) * n
  }.sum

  @tailrec
  def highestFiveCardinal(decimal: BigInt, x: Int = 0): Int =
    if (decimal / BigInt(5).pow(x) < BigInt(1)) x
    else highestFiveCardinal(decimal, x + 1)

  @tailrec
  def convertToSnafu(decimal: BigInt, x: Int, result: List[Int] = List(0)): String =
    if (x < 0) result.reverse.dropWhile(_ == 0).map {
      case -1 => '-'
      case -2 => '='
      case x => x.toString.head
    }.mkString

    else {
      val div = decimal / BigInt(5).pow(x)
      val rem = decimal % BigInt(5).pow(x)

      convertToSnafu(rem, x - 1, propagate(div.toInt, result))
    }

  def propagate(value: Int, xs: List[Int]): List[Int] = xs match
    case Nil => List(value)
    case head :: tail =>
      val (left, current) = value match
        case 3 | 4 => (1, value - 5)
        case _ => (0, value)

      val result = current :: propagate(left + head, tail)
      result

  def toSnafu(decimal: BigInt): String = convertToSnafu(decimal, highestFiveCardinal(decimal))

  val input = readFile("resources/day25")

  val res1 = toSnafu(input.map(toDecimal).sum)

  println(s"Part 1: $res1")
}