import Util.readFile

import scala.annotation.tailrec

@main def day22(): Unit = {

  val input = readFile("resources/day22")

  val rawMap = input.dropRight(2).map(_.toIndexedSeq).zipWithIndex.map { case (a, b) => (b, a) }.toMap

  val maxLength = rawMap.values.map(_.length).max

  val fullMap = rawMap.map { case (k, v) => k -> v.padTo(maxLength, ' ') }

  sealed trait Command
  case class Move(steps: Int) extends Command
  case class Turn(direction: Char) extends Command {
    val counterClockwiseTurns: Int = direction match
      case 'L' => 3
      case 'R' => 1
  }

  @tailrec
  def parseInstructions(s: String, acc: List[Command] = Nil): List[Command] = s.headOption match
    case None =>
      acc

    case Some(x) if x.isDigit =>
      parseInstructions(
        s.dropWhile(_.isDigit),
        acc.appended(Move(s.takeWhile(_.isDigit).toInt))
      )

    case Some(x) => acc.appended(Turn(x))
      parseInstructions(s.tail, acc.appended(Turn(x)))

  val instructions = parseInstructions(input.last)

  def printmap(map: Map[Int, IndexedSeq[Char]]): Unit = {
    println((0 until map.size).map { x =>
      map(x).mkString
    }.mkString("\n"))
  }

  def turnMapCounterClockwise(map: Map[Int, IndexedSeq[Char]]): Map[Int, IndexedSeq[Char]] = map(0).indices.map { x =>
    (0 until map.size).map { y =>
      map(y)(map(0).size - x - 1)
    }
  }.zipWithIndex.map { case (a, b) => (b, a) }.toMap

  def turnCounterClockwise(x: Int, y: Int, n: Int, m: Int) =
    (m - y - 1, x, m, n)

  def turnedCoordinates(x: Int, y: Int, n: Int, m: Int, turn: Turn): (Int, Int, Int, Int) =
    (0 until turn.counterClockwiseTurns).foldLeft((x, y, n, m)) {
      case ((x2, y2, n2, m2), _) => turnCounterClockwise(x2, y2, n2, m2)
    }

  val turnedMaps = (0 until 3).foldLeft(IndexedSeq(fullMap)) { case (acc, _) =>
    acc.appended(turnMapCounterClockwise(acc.last))
  }

  def turnedMapIndex(i: Int, turn: Turn) = (i + 4 + turn.counterClockwiseTurns) % 4

  case class State(orientation: Int, x: Int, y: Int, instructions: List[Command]) {
    val n: Int = turnedMaps(orientation).size
    val m: Int = turnedMaps(orientation).values.head.length

    val (x0, y0, _, _) = (0 until (4 - orientation)).foldLeft((x, y, n, m)) {
      case ((x2, y2, n2, m2), _) => turnCounterClockwise(x2, y2, n2, m2)
    }

    val password = 1000 * (x0 + 1) + 4 * (y0 + 1) + orientation
  }

  val s0 = State(0, 0, turnedMaps(0)(0).indexWhere(_ == '.'), instructions)

  def nextLine1(orientation: Int, line: Int): (Int, Int) = orientation -> line

  val side = 50

  def nextLine2(orientation: Int, line: Int): (Int, Int) = orientation match
    case 0 =>
      line / side match
        case 0 => 2 -> (line + side)
        case 1 => 3 -> (line + side)
        case 2 => 2 -> (line + side)
        case _ => 3 -> (line - 2 * side)
    case 1 =>
      line / side match
        case 0 => 2 -> (line + 2 * side)
        case 1 => 2 -> (line - side)
        case _ => 1 -> (line - 2 * side)
    case 2 =>
      line / side match
        case 0 => 1 -> (line + side)
        case 1 => 0 -> (line - side)
        case 2 => 1 -> line
        case _ => 0 -> (line - side)
    case _ =>
      line / side match
        case 0 => 0 -> (line + side)
        case 1 => 0 -> (line + 2 * side)
        case _ => 3 -> (line - 2 * side)

  val sideExample = 4

  def nextLine2Example(orientation: Int, line: Int): (Int, Int) = orientation match
    case 0 =>
      line / side match
        case 0 => 2 -> line
        case 1 => 1 -> (line - side)
        case _ => 2 -> line
    case 1 =>
      line / side match
        case 0 => 0 -> (line + side)
        case 1 => 3 -> (line - side)
        case 2 => 0 -> line
        case _ => 3 -> (line - side)
    case 2 =>
      line / side match
        case 0 => 3 -> (line + side)
        case 1 => 3 -> (line + 2 * side)
        case _ => 1 -> line
    case _ =>
      line / side match
        case 0 => 1 -> (line + side)
        case 1 => 0 -> (line - side)
        case 2 => 1 -> (line + side)
        case _ => 2 -> (line - 2 * side)

  @tailrec
  def reduceState(s: State, nextLineFunction: (Int, Int) => (Int, Int)): State = {
    s.instructions match
      case Nil => s
      case Move(steps) :: cs =>
        val fullLine = turnedMaps(s.orientation)(s.x)
        val prefixLength = fullLine.takeWhile(_ == ' ').length
        val croppedLine = fullLine.drop(prefixLength).takeWhile(_ != ' ')
        val croppedIndex = s.y - prefixLength
        val firstWall = croppedLine.drop(croppedIndex).indexWhere(_ == '#')
        val newState =
          if (firstWall == -1) {
            // No walls ahead
            if (steps <= croppedLine.size - 1 - croppedIndex) {
              // Can do all the steps
              s.copy(y = croppedIndex + steps + prefixLength, instructions = cs)
            } else {
              // Need to wrap
              val (nextOrientation, nextX) = nextLineFunction(s.orientation, s.x)
              val nextFullLine = turnedMaps(nextOrientation)(nextX)
              val nextPrefixLength = nextFullLine.takeWhile(_ == ' ').length
              val nextLine = nextFullLine.drop(nextPrefixLength).takeWhile(_ != ' ')
              if (nextLine.head == '#') {
                // Can't move there
                s.copy(y = croppedLine.size + prefixLength - 1, instructions = cs)
              } else {
                // Can move there
                s.copy(
                  orientation = nextOrientation,
                  x = nextX,
                  y = nextPrefixLength,
                  instructions = Move(steps - (croppedLine.size - croppedIndex)) :: cs
                )
              }
            }
          } else {
            // There's a wall ahead
            val newPosition = Math.min(firstWall - 1, steps) + prefixLength + croppedIndex
            s.copy(y = newPosition, instructions = cs)
          }
        reduceState(newState, nextLineFunction)
      case (t@Turn(_)) :: cs =>
        val (x, y, _, _) = turnedCoordinates(s.x, s.y, s.n, s.m, t)
        val newState = s.copy(orientation = turnedMapIndex(s.orientation, t), x = x, y = y, instructions = cs)
        reduceState(newState, nextLineFunction)
  }

  val reducedState1 = reduceState(s0, nextLine1)
  println(s"Part 1: ${reducedState1.password}")

  val reducedState2 = reduceState(s0, nextLine2)
  println(s"Part 2: ${reducedState2.password}")
}