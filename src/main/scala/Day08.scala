import Util.readFile

import scala.annotation.tailrec

@main def day08(): Unit = {

  val input = readFile("resources/day08").map(_.map(_.toString.toInt)).toIndexedSeq

  // Part 1

  def getVisibleFromNorth(xs: IndexedSeq[IndexedSeq[Int]]): Set[(Int, Int)] = {
    val (visibleTrees, _) = xs.zipWithIndex.foldLeft(
      xs.indices.map(0 -> _),
      xs.head
    ) {
      case ((visible, maximums), (line, i)) =>
        (
          visible ++
            line.zip(maximums).zipWithIndex.flatMap {
              case ((a, b), j) =>
                if (a > b) Some(i -> j)
                else None
            },
          line.zip(maximums).map(Math.max(_, _))
        )
    }

    visibleTrees.toSet
  }

  def turn(xs: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = xs.indices.map { x =>
    xs.indices.map { y =>
      xs(y)(xs.size - 1 - x)
    }
  }

  @tailrec
  def reverse(x: Int, y: Int, times: Int): (Int, Int) = {
    if (times < 1) (x, y)
    else reverse(y, input.size - 1 - x, times - 1)
  }

  val (_, visibleFromAnywhere) = (0 until 4).foldLeft(input, Set.empty[(Int, Int)]) {
    case ((state, visible), times) =>
      (turn(state), visible.union {
        getVisibleFromNorth(state).map {
          case (x, y) => reverse(x, y, times)
        }
      })
  }

  println(visibleFromAnywhere.size)

  // Part 2

  def getScore(i: Int, j: Int): Int = {
    val h = input(i)(j)
    getScoreDirection(i - 1, j, h, -1, 0, 0) *
      getScoreDirection(i + 1, j, h, 1, 0, 0) *
      getScoreDirection(i, j - 1, h, 0, -1, 0) *
      getScoreDirection(i, j + 1, h, 0, 1, 0)
  }

  @tailrec
  def getScoreDirection(i: Int, j: Int, height: Int, right: Int, down: Int, acc: Int): Int = {
    if (i < 0 || i >= input.size || j < 0 || j >= input.size) acc
    else if (input(i)(j) >= height) acc + 1
    else getScoreDirection(i + right, j + down, height, right, down, acc + 1)
  }

  val highestScore = (for {
    i <- input.indices
    j <- input.indices
  } yield getScore(i, j)).max

  println(highestScore)
}

