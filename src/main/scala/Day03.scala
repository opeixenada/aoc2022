import Util.readFile

import scala.annotation.tailrec

@main def day03(): Unit = {

  val input = readFile("resources/day03")

  // Part 1

  def getError(s: String): Char = {
    val (left, right) = s.splitAt(s.length / 2)
    left.toSet.intersect(right.toSet).head
  }

  def getPriority(char: Char): Int = if (char.isLower) char - 'a' + 1 else char - 'A' + 27

  val r1 = input.foldLeft(0) {
    case (x, rucksack) => x + getPriority(getError(rucksack))
  }

  println(r1)

  // Part 2

  @tailrec
  def splitToTriples[A](xs: List[A], ys: List[List[A]] = Nil): List[List[A]] = xs match {
    case Nil => ys
    case _ => splitToTriples(xs.drop(3), xs.take(3) :: ys)
  }

  val r2 = splitToTriples(input).foldLeft(0) {
    case (x, triple) => x + getPriority(triple.map(_.toSet).reduce(_.intersect(_)).head)
  }

  println(r2)
}