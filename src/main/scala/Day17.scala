import Util.readFile

import scala.annotation.tailrec

@main def day17(): Unit = {

  val input = readFile("resources/day17").head.toList

  val jets = LazyList.continually(input.to(LazyList)).flatten

  // Part 1

  val rocks: IndexedSeq[Set[(Int, Int)]] = List(
    """####""",
    """|.#.
       |###
       |.#.""".stripMargin,
    """|..#
       |..#
       |###""".stripMargin,
    """|#
       |#
       |#
       |#""".stripMargin,
    """|##
       |##""".stripMargin
  )
    .map { rock =>
      for {
        (s, y) <- rock.split('\n').reverse.zipWithIndex
        (ch, x) <- s.zipWithIndex
        if ch == '#'
      } yield (x + 2, y)
    }
    .map(_.toSet)
    .toIndexedSeq

  case class State(
                    chamber: Set[(Int, Int)] = Set.empty,
                    rock: Set[(Int, Int)] = Set.empty,
                    diff: Int = 0,
                    offset: BigInt = BigInt(0)
                  ) {

    val maxYChamber: Int = chamber.map(_._2).maxOption.getOrElse(-1)

    @tailrec
    final def fall(jets: LazyList[Char], jetPosition: Int): (State, LazyList[Char], Int) = {
      // Move rock by jet

      val movedRock = jets.head match
        case '<' =>
          val cantMove = rock.exists { case (rx, ry) =>
            chamber.contains(rx - 1, ry + diff) || rx == 0
          }
          if (cantMove) rock
          else rock.map { case (rx, ry) => (rx - 1, ry) }
        case '>' =>
          val cantMove = rock.exists { case (rx, ry) =>
            chamber.contains(rx + 1, ry + diff) || rx == 6
          }
          if (cantMove) rock
          else rock.map { case (rx, ry) => (rx + 1, ry) }

      if (diff == 0 || movedRock.exists { case (rx, ry) =>
        chamber.contains(rx, ry + diff - 1)
      }) {
        // Rock stops
        val rockPosition = movedRock.map { case (rx, ry) =>
          (rx, ry + diff)
        }

        val newChamber = chamber ++ rockPosition

        val (newOffset, trimmedChamber): (Int, Set[(Int, Int)]) = rockPosition.map(_._2)
          .filter { y => (0 to 6).forall(x => newChamber.contains((x, y))) }
          .maxOption
          .map { newFloor =>
            val trimmedChamber = newChamber
              .filter(_._2 > newFloor)
              .map { case (x, y) =>
                (x, y - newFloor - 1)
              }
            (newFloor + 1) -> trimmedChamber
          }
          .getOrElse(0 -> newChamber)

        (State(chamber = trimmedChamber, offset = offset + newOffset), jets.tail, jetPosition + 1)
      }

      else {
        // Rock keeps falling
        State(chamber, movedRock, diff - 1, offset).fall(jets.tail, jetPosition + 1)
      }
    }

    def addRock(newRock: Set[(Int, Int)]): State = {
      this.copy(
        rock = newRock,
        diff = maxYChamber + 4,
      )
    }

    def view: String = {
      val maxYRock = rock.map(_._2 + diff).maxOption.getOrElse(0)

      (0 to Math.max(maxYChamber, maxYRock)).map { y =>
        s"$y".padTo(4, ' ') + (0 to 6).map { x =>
          if (chamber.contains(x -> y)) '#'
          else if (rock.contains(x -> (y - diff))) '@'
          else '.'
        }.mkString("")
      }.reverse.mkString("\n")
    }
  }

  val lastRock = 2022

  val (endState, _, _) = (BigInt(0) until lastRock).foldLeft((State(), jets, 0)) {
    case ((state, js, jp), i) =>
      val rock = rocks(i.mod(5).toInt)
      val (result, leftJets, newJetPosition) = state.addRock(rock).fall(js, jp)
      (result, leftJets, newJetPosition)
  }

  println(endState.maxYChamber + endState.offset + 1)

  // Part 2

  /**
   * Every 1700 rocks (440, 2140, etc.) the pattern repeats.
   * The point in the jets pattern is 2602.
   * Offset: 671 + 2623 * n.
   */

  val lastRock2 = BigInt("1000000000000")
  val lastRockAfterOffset = (lastRock2 - 440) % 1700
  val offsetCount = (lastRock2 - 440) / 1700

  val (endState2, _) = (BigInt(0) until lastRockAfterOffset).foldLeft((State(), jets.drop(2603))) {
    case ((state, js), i) =>
      val rock = rocks(i.mod(5).toInt)
      val (result, leftJets, _) = state.addRock(rock).fall(js, 0)
      (result, leftJets)
  }

  println(endState2.maxYChamber + endState2.offset + 1 + 671 + offsetCount * 2623)
}
