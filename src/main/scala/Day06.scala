import Util.readFile

import scala.annotation.tailrec

@main def day06(): Unit = {

  val input = readFile("resources/day06").head

  @tailrec
  def findMarker(stack: List[Char], s: String, index: Int, length: Int): Int = {
    if ((s.head :: stack.take(length - 1)).distinct.length == length) index + 1
    else findMarker(s.head :: stack.take(length - 1), s.tail, index + 1, length)
  }

  // Part 1

  println(findMarker(Nil, input, 0, 4))

  // Part 2

  println(findMarker(Nil, input, 0, 14))
}