import Util.readFile

import scala.annotation.tailrec

@main def day13(): Unit = {

  sealed trait Token
  case object Open extends Token
  case class Lst(xs: List[Token]) extends Token
  case class IntVal(x: Int) extends Token

  def compareTokens(x: Token, y: Token): Int = {
    (x, y) match
      case (IntVal(a), IntVal(b)) => a.compare(b)
      case (Lst(xs), Lst(ys)) =>
        val result = xs.zip(ys).foldLeft(0) {
          case (0, (a, b)) => compareTokens(a, b)
          case (acc, _) => acc
        }

        if (result == 0) xs.length.compare(ys.length)
        else result
      case (a, Lst(ys)) => compareTokens(Lst(List(a)), Lst(ys))
      case (Lst(xs), b) => compareTokens(Lst(xs), Lst(List(b)))
      case _ => 0
  }

  val input = readFile("resources/day13")

  // Part 1

  @tailrec
  def parse(xs: List[String], ys: List[(Token, Token)] = Nil): List[(Token, Token)] = xs match {
    case a :: b :: Nil => ys.appended(parseLst(a.toList) -> parseLst(b.toList))
    case a :: b :: _ :: d => parse(d, ys.appended(parseLst(a.toList) -> parseLst(b.toList)))
  }

  @tailrec
  def parseLst(s: List[Char], currentValue: Option[String] = None, stack: List[Token] = Nil): Token = {
    s match
      case '[' :: xs =>
        parseLst(xs, currentValue, Open :: stack)

      case x :: xs if x.isDigit =>
        parseLst(xs, currentValue.map(_.appended(x)).orElse(Some(s"$x")), stack)

      case ',' :: xs =>
        parseLst(xs, None, stack.prependedAll(currentValue.map { x => IntVal(x.toInt) }))

      case ']' :: xs =>
        val newStack = stack.prependedAll(currentValue.map { x => IntVal(x.toInt) })
        val enclosedValues = newStack.takeWhile(_ != Open)
        parseLst(xs, None, Lst(enclosedValues.reverse) :: newStack.drop(enclosedValues.length + 1))

      case _ =>
        stack.head
  }

  val parsedInput = parse(input)

  val result = parsedInput.zipWithIndex.foldLeft(0) {
    case (acc, ((x, y), i)) =>
      if (compareTokens(x, y) == -1) acc + 1 + i
      else acc
  }

  println(result)

  // Part 2

  implicit val TokenOrdering: Ordering[Token] = new Ordering[Token] {
    override def compare(x: Token, y: Token): Int = {
      compareTokens(x, y)
    }
  }

  val divider1 = parseLst("[[2]]".toList)
  val divider2 = parseLst("[[6]]".toList)

  val sortedPackets = parsedInput
    .flatMap { x => List(x._1, x._2) }
    .appendedAll(List(divider1, divider2))
    .sorted

  println((sortedPackets.indexOf(divider1) + 1) * (sortedPackets.indexOf(divider2) + 1))
}