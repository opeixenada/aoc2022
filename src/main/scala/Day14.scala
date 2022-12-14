import Util.readFile

import scala.annotation.tailrec

@main def day14(): Unit = {

  val coordinates = readFile("resources/day14").map { s =>
    s.split(" -> ").map { cs =>
      val splitCs = cs.split(',').map(_.toInt)
      splitCs.head -> splitCs.last
    }.toList
  }

  // Part 1

  val rockUnits = coordinates.flatMap { line =>
    line.zip(line.tail).flatMap {
      case ((x1, y1), (x2, y2)) =>
        for {
          x <- Math.min(x1, x2) to Math.max(x1, x2)
          y <- Math.min(y1, y2) to Math.max(y1, y2)
        } yield (x, y)
    }
  }.toSet

  val bottomLevel = rockUnits.map(_._2).max

  @tailrec
  def addMoreSand(count: Int, filled: Set[(Int, Int)]): Int = {
    val (newState, stable) = addSand(500, 0, filled)
    if (stable) addMoreSand(count + 1, newState)
    else count
  }

  @tailrec
  def addSand(x: Int, y: Int, filled: Set[(Int, Int)]): (Set[(Int, Int)], Boolean) = {
    if (y >= bottomLevel) (filled, false)
    else if (!filled.contains(x -> (y + 1))) addSand(x, y + 1, filled)
    else if (!filled.contains((x - 1) -> (y + 1))) addSand(x - 1, y + 1, filled)
    else if (!filled.contains((x + 1) -> (y + 1))) addSand(x + 1, y + 1, filled)
    else (filled + (x -> y), true)
  }

  val res1 = addMoreSand(0, rockUnits)

  println(res1)

  // Part 2

  val lastLevel = bottomLevel + 1

  @tailrec
  def addMoreSand2(count: Int, filled: Set[(Int, Int)]): Int = {
    val (newState, stable) = addSand2(500, 0, filled)
    if (stable) addMoreSand2(count + 1, newState)
    else count + 1
  }

  @tailrec
  def addSand2(x: Int, y: Int, filled: Set[(Int, Int)]): (Set[(Int, Int)], Boolean) = {
    if (y == lastLevel) (filled + (x -> y), true)
    else if (!filled.contains(x -> (y + 1))) addSand2(x, y + 1, filled)
    else if (!filled.contains((x - 1) -> (y + 1))) addSand2(x - 1, y + 1, filled)
    else if (!filled.contains((x + 1) -> (y + 1))) addSand2(x + 1, y + 1, filled)
    else if (y > 0) (filled + (x -> y), true)
    else (filled, false)
  }

  val res2 = addMoreSand2(0, rockUnits)

  println(res2)
}