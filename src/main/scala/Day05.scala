import Util.readFile

import scala.annotation.tailrec

@main def day05(): Unit = {

  val input = readFile("resources/day05")

  val initialStateInput = input.takeWhile(_.nonEmpty)
  val commandsInput = input.drop(initialStateInput.length + 1)

  @tailrec
  def buildStacks(xs: Map[Int, String], s: String, index: Int = 0): Map[Int, String] = {
    if (s.isEmpty) xs
    else {
      val ch = s.tail.head
      val newMap = if (ch.isLetter) xs + (index -> (xs.getOrElse(index, "") + ch)) else xs
      buildStacks(newMap, s.drop(4), index + 1)
    }
  }

  val stacks = initialStateInput.dropRight(1).foldLeft(Map.empty[Int, String]) {
    buildStacks(_, _)
  }

  val commandPattern = """move (\d+) from (\d+) to (\d+)""".r

  def move(stacks: Map[Int, String], count: Int, from: Int, to: Int, reverse: Boolean): Map[Int, String] = {
    val xs = stacks(from).take(count)
    stacks + (from -> stacks(from).drop(count)) + (to -> ((if (reverse) xs.reverse else xs) + stacks(to)))
  }

  def getTops(reverse: Boolean): String = commandsInput.foldLeft(stacks) { case (acc, s) =>
    val commandPattern(count, from, to) = s
    move(acc, count.toInt, from.toInt - 1, to.toInt - 1, reverse)
  }.toList.sortBy(_._1).flatMap(_._2.headOption).mkString

  // Part 1

  println(getTops(true))

  // Part 2

  println(getTops(false))
}