import Util.readFile

import scala.annotation.tailrec

@main def day23(): Unit = {

  val input = readFile("resources/day23")

  val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))

  val nextDirections: Map[(Int, Int), (Int, Int)] = directions.zip(directions.tail.appended(directions.head)).toMap

  val elvesSet: Set[(Int, Int)] = (for {
    (line, x) <- input.zipWithIndex
    (_, y) <- line.zipWithIndex.filter(_._1 == '#')
  } yield (x, y)).toSet

  def propose(x: Int, y: Int, firstDirection: (Int, Int), elvesCs: Set[(Int, Int)]): (Int, Int) = {
    val hasNeighbors = (for {
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 || j != 0
    } yield (x + i, y + j)).exists { cs => elvesCs.contains(cs._1, cs._2) }

    if (!hasNeighbors) (x, y)
    else {
      (0 until 4).foldLeft((firstDirection, List[(Int, Int)]())) { case ((dir, acc), _) =>

        val directionEmpty = dir match
          case (0, j) => (-1 to 1).forall { i => !elvesCs.contains((i + x, j + y)) }
          case (i, _) => (-1 to 1).forall { j => !elvesCs.contains((i + x, j + y)) }

        val nextDirection = nextDirections(dir)
        nextDirection -> (if (directionEmpty) acc.appended((x + dir._1, y + dir._2)) else acc)
      }._2.headOption.getOrElse(x -> y)
    }
  }

  @tailrec
  def step(elves: Set[(Int, Int)], firstDirection: (Int, Int), n: Int): (Set[(Int, Int)], (Int, Int)) = {
    if (n == 0) (elves, firstDirection)
    else {

      val map = elves.toSeq.map { case (x, y) =>
        val (x1, y1) = propose(x, y, firstDirection, elves)
        (x, y, x1, y1)
      }.groupBy(t => t._3 -> t._4)

      val endPositions = map.values.flatten.map { t =>
        if (map(t._3 -> t._4).length > 1) {
          // No step
          t._1 -> t._2
        } else {
          t._3 -> t._4
        }
      }

      step(endPositions.toSet, nextDirections(firstDirection), n - 1)
    }
  }

  @tailrec
  def converge(elves: Set[(Int, Int)], firstDirection: (Int, Int), n: Int): Int = {

   val afterStep = step(elves, firstDirection, 1)

    if (afterStep._1 == elves) n + 1
    else converge(afterStep._1, afterStep._2, n + 1)
  }

  val (result, _) = step(elvesSet, directions.head, 10)

  def emptyTiles(elves: Set[(Int, Int)]): Int = {
    val xs = elves.map(_._1)
    val ys = elves.map(_._2)

    (for {
      x <- xs.min to xs.max
      y <- ys.min to ys.max
      if !elves.contains(x -> y)
    } yield 1).sum
  }

  println(s"Part 1: ${emptyTiles(result)}")
  println(s"Part 2: ${converge(elvesSet, directions.head, 0)}")
}