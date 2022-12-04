import Util.readFile

import scala.annotation.tailrec
import scala.io.Source

@main def day04(): Unit = {

  val input = readFile("resources/day04")

  // Part 1

  val pattern = """(\d+)-(\d+),(\d+)-(\d+)""".r

  val completelyOverlappingPairs = input.foldLeft(0) {
    case (count, s) =>
      val pattern(a, b, c, d) = s
      count + (if ((a.toInt >= c.toInt && b.toInt <= d.toInt) || (a.toInt <= c.toInt && b.toInt >= d.toInt)) 1 else 0)
  }

  println(completelyOverlappingPairs)

  // Part 2

  val overlappingPairs = input.foldLeft(0) {
    case (count, s) =>
      val pattern(a, b, c, d) = s
      count + (if ((a.toInt > d.toInt) || (b.toInt < c.toInt)) 0 else 1)
  }

  println(overlappingPairs)
}