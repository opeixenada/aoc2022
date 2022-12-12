import Util.readFile

import scala.annotation.tailrec

@main def day12(): Unit = {

  val input = readFile("resources/day12").toIndexedSeq

  // Part 1

  val input2 = input.map {
    _.map {
      case 'S' => 'a'
      case 'E' => 'z'
      case x => x
    }
  }

  def neighborsUp(x: Int, y: Int) = List((x - 1) -> y, (x + 1) -> y, x -> (y - 1), x -> (y + 1)).filter { (a, b) =>
    a > -1 && a < input.head.length && b > -1 && b < input.size && ((input2(b)(a) - input2(y)(x)) < 2)
  }

  def find(char: Char) = (
    for {
      x <- input.head.indices
      y <- input.indices
      if input(y)(x) == char
    } yield (x, y)
    ).head

  val start = find('S')
  val end = find('E')

  @tailrec
  def findPath(visited: Set[(Int, Int)], front: Set[(Int, Int)], count: Int): Int = {
    if (front.contains(end)) count
    else findPath(visited.union(front), front.flatMap { (x, y) => neighborsUp(x, y) }.diff(visited), count + 1)
  }

  println(findPath(Set(start), Set(start), 0))

  // Part 2

  def neighborsDown(x: Int, y: Int) = List((x - 1) -> y, (x + 1) -> y, x -> (y - 1), x -> (y + 1)).filter { (a, b) =>
    a > -1 && a < input.head.length && b > -1 && b < input.size && ((input2(y)(x) - input2(b)(a)) < 2)
  }

  @tailrec
  def findPath2(visited: Set[(Int, Int)], front: Set[(Int, Int)], count: Int): Int = {
    if (front.exists { (x, y) => input2(y)(x) == 'a' }) count
    else findPath2(visited.union(front), front.flatMap { (x, y) => neighborsDown(x, y) }.diff(visited), count + 1)
  }

  println(findPath2(Set(end), Set(end), 0))
}