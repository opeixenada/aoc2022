import Util.readFile
import sun.security.util.Length

import scala.util.matching.Regex

@main def day09(): Unit = {

  val pattern: Regex = """(.) (\d+)""".r

  val input = readFile("resources/day09").map {
    case pattern(x, y) => x.head -> y.toInt
  }

  def plus(a: (Int, Int), b: (Int, Int)) = (a._1 + b._1) -> (a._2 + b._2)

  def countTailPositions(knotsCount: Int) = {

    val (_, tailVisited) = input.foldLeft(List.fill(knotsCount)(0 -> 0), Set(0 -> 0)) {
      case ((ks, v), (direction, steps)) =>

        val move = direction match
          case 'R' => 1 -> 0
          case 'U' => 0 -> 1
          case 'L' => -1 -> 0
          case 'D' => 0 -> -1

        (0 until steps).foldLeft(ks, v) {
          case ((knots, visited), _) =>

            val newKnots = knots.tail.foldLeft(List(plus(knots.head, move))) {
              case (leaders, follower) =>
                val leader = leaders.last
                val newFollower = (leader._1 - follower._1, leader._2 - follower._2) match
                  case (a, b) if a.abs < 2 && b.abs < 2 => follower
                  case (a, b) => plus(follower, a.sign -> b.sign)

                leaders.appended(newFollower)
            }

            (newKnots, visited + newKnots.last)
        }
    }

    tailVisited.size
  }

  // Part 1

  println(countTailPositions(2))

  // Part 2

  println(countTailPositions(10))
}

