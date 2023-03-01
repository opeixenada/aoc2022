import Util.readFile

import scala.annotation.tailrec

@main def day24(): Unit = {

  val input = readFile("resources/day24")

  val maxX: Int = input.head.length - 1
  val maxY: Int = input.length - 1
  val start: (Int, Int) = (1, 0)
  val goal: (Int, Int) = (maxX - 1, maxY)

  val blizzards0 = input.zipWithIndex.flatMap { case (str, y) =>
    str.zipWithIndex.map { case (ch, x) =>
      (x, y, ch)
    }
  }.filter(b => Set('v', '>', '^', '<').contains(b._3))

  def nextPositions(x0: Int, y0: Int, bs: Seq[(Int, Int, Char)]): Seq[(Int, Int)] =
    for {
      x <- (x0 - 1) to (x0 + 1)
      y <- (y0 - 1) to (y0 + 1)
      if x == x0 || y == y0 // Moves can not be diagonal
      if (x, y) == start || (x, y) == goal || (x > 0 && x < maxX && y > 0 && y < maxY) // No walls
      if !bs.exists(b => b._1 == x && b._2 == y) // No blizzards
    } yield (x, y)

  def nextBlizzards(bs: Seq[(Int, Int, Char)]): Seq[(Int, Int, Char)] = bs.map { case (x, y, b) =>
    b match
      case '>' =>
        if (x + 1 < maxX) (x + 1, y, b)
        else (1, y, b)

      case '<' =>
        if (x - 1 > 0) (x - 1, y, b)
        else (maxX - 1, y, b)

      case '^' =>
        if (y - 1 > 0) (x, y - 1, b)
        else (x, maxY - 1, b)

      case 'v' =>
        if (y + 1 < maxY) (x, y + 1, b)
        else (x, 1, b)
  }

  case class State(e: (Int, Int), blizzards: Seq[(Int, Int, Char)]) {

    def getBlizzards(x: Int, y: Int): Seq[Char] = blizzards.filter(b => b._1 == x && b._2 == y).map(_._3)

    override def toString: String = {
      (0 to maxY).map { y =>
        (0 to maxX).map { x =>
          if ((x == 0 || y == 0 || x == maxX || y == maxY) && (x, y) != start && (x, y) != goal) '#'
          else if (e == (x, y)) 'E'
          else {
            val bs = getBlizzards(x, y)
            bs.length match {
              case 0 => '.'
              case 1 => bs.head
              case n => n.toString.head
            }
          }
        }.mkString
      }.mkString("\n")
    }

    def next(): Seq[State] = {
      val bs = nextBlizzards(blizzards)
      nextPositions(e._1, e._2, bs).map { position =>
        copy(e = position, blizzards = bs)
      }
    }
  }

  val s0 = State(
    e = start,
    blizzards = blizzards0,
  )

  @tailrec
  def search(states: List[State], isTerminal: State => Boolean, minute: Int = 0): (State, Int) = {
    states.find(isTerminal) match
      case Some(s) => s -> minute
      case _ => search(states.flatMap(_.next()).distinct, isTerminal, minute + 1)
  }

  val (s1, min1) = search(List(s0), isTerminal = { (s: State) => s.e == goal })
  val (s2, min2) = search(List(s1), isTerminal = { (s: State) => s.e == start })
  val (_, min3) = search(List(s2), isTerminal = { (s: State) => s.e == goal })

  println(s"Part 1: $min1")
  println(s"Part 2: ${min1 + min2 + min3}")
}