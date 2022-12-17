import Util.readFile

import scala.annotation.tailrec
import scala.util.matching.Regex

@main def day15(): Unit = {
  val pattern: Regex = """Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)""".r

  val input = readFile("resources/day15").map {
    case pattern(xs, ys, xb, yb) => (xs.toInt, ys.toInt, xb.toInt, yb.toInt)
  }

  // Part 1

  def beaconsOnLine(line: Int): Set[Int] = input.flatMap {
    case (_, _, xb, yb) if yb == line => Some(xb)
    case _ => None
  }.toSet

  @tailrec
  def intersect(intervals: List[(Int, Int)], a: Int, b: Int, done: List[(Int, Int)]): List[(Int, Int)] =
    intervals match {
      case Nil => (a -> b) :: done
      case (x, y) :: xs if a == y + 1 => intersect(xs, x, b, done)
      case (x, y) :: xs if b == x - 1 => intersect(xs, a, y, done)
      case (x, y) :: xs if a <= y && b >= x => intersect(xs, Math.min(a, x), Math.max(b, y), done)
      case head :: xs => intersect(xs, a, b, head :: done)
    }

  @tailrec
  def split(intervals: List[(Int, Int)], a: Int, done: List[(Int, Int)]): List[(Int, Int)] =
    intervals match {
      case Nil => done
      case (x, y) :: xs if a == x => (x + 1, y) :: xs
      case (x, y) :: xs if a == y => (x, y - 1) :: xs
      case (x, y) :: xs if a > x && a < y => (x, a - 1) :: (a + 1, y) :: xs
      case head :: xs => split(xs, a, head :: done)
    }

  def countSize(ranges: List[(Int, Int)]): Int = ranges.foldLeft(0) { case (acc, (a, b)) =>
    acc + (b - a + 1)
  }

  def part1(line: Int): Int = {

    val whereBeaconCanNotBe = input.foldLeft(List.empty[(Int, Int)]) {
      case (acc, (xs, ys, xb, yb)) =>
        val d = (xs - xb).abs + (ys - yb).abs
        val yd = (ys - line).abs
        val xd = d - yd
        if (xd < 0) acc
        else intersect(acc, xs - xd, xs + xd, Nil)
    }

    val intervals = beaconsOnLine(line).foldLeft(whereBeaconCanNotBe) {
      case (acc, beacon) =>
        split(acc, beacon, Nil)
    }

    countSize(intervals)
  }

  println(part1(2000000))

  // Part 2

  val min = 0
  val max = 4000000

  def part2(line: Int): Option[Int] = {

    val whereBeaconCanNotBe = input.foldLeft(List.empty[(Int, Int)]) {
      case (acc, (xs, ys, xb, yb)) =>
        val d = (xs - xb).abs + (ys - yb).abs
        val yd = (ys - line).abs
        val xd = d - yd

        if (xd < 0) acc
        else if (xs + xd < min || xs - xd > max) acc
        else intersect(acc, Math.max(xs - xd, min), Math.min(xs + xd, max), Nil)
    }

    if (whereBeaconCanNotBe == List(min -> max)) None
    else Some(whereBeaconCanNotBe.map(_._2).min + 1)
  }

  def tuningFrequency(x: Int, y: Int): BigInt = BigInt(x) * 4000000 + y

  (min to max).foreach { y =>
    part2(y).foreach { x =>
      println(tuningFrequency(x, y))
    }
  }
}