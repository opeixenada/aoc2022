import Util.readFile

import scala.annotation.tailrec
import scala.util.matching.Regex

@main def day16(): Unit = {

  val pattern: Regex = """Valve (.+) has flow rate=(.+); tunnels? leads? to valves? (.+)""".r

  case class Valve(id: String, rate: Int, paths: List[String])

  val valves: Map[String, Valve] = readFile("resources/day16").map {
    case pattern(id, rate, paths) => Valve(id, rate.toInt, paths.split(", ").toList)
  }.map { x => x.id -> x }.toMap

  val nonZeroValves = valves.filter(_._2.rate > 0).keys.toList

  @tailrec
  def findMinPathLengths(pathLengths: Map[String, Int]): Map[String, Int] = {
    if (pathLengths.size == valves.size) pathLengths
    else findMinPathLengths(pathLengths.concat {
      for {
        (k, v) <- pathLengths
        next <- valves(k).paths
        if !pathLengths.get(next).exists(_ < v + 1)
      } yield next -> (v + 1)
    })
  }

  val minPathLengths = ("AA" :: valves.values.toList.map(_.id)).map { start =>
    start -> findMinPathLengths(Map(start -> 0))
  }.toMap

  case class MyTrajectory(openedValves: Map[String, Int], maxTime: Int) {

    val time: Int = openedValves.values.maxOption.getOrElse(0)

    val releasedPressure: Int = openedValves.foldLeft(0) {
      case (acc, (k, v)) => acc + (if (v < maxTime) valves(k).rate * (maxTime - v) else 0)
    }

    val signature: String = {
      val lastValve = openedValves.find(_._2 == time).map(_._1).getOrElse("")
      val valves = openedValves.keys.toList.sorted.mkString(".")
      s"$lastValve|$valves"
    }
  }

  def myMovements(tr: MyTrajectory, filter: List[String] = Nil): List[MyTrajectory] = {
    for {
      myGoal <- nonZeroValves.filterNot(tr.openedValves.contains).filterNot(filter.contains)
      myCurrentPosition = tr.openedValves.find(_._2 == tr.time).map(_._1).getOrElse("AA")
      myDistance = minPathLengths(myCurrentPosition)(myGoal)
      myNewTime = tr.time + myDistance + 1
      if myNewTime < tr.maxTime
    } yield {
      tr.copy(openedValves = tr.openedValves + (myGoal -> myNewTime))
    }
  }

  @tailrec
  def findMyMaxScore(
                      states: List[MyTrajectory],
                      traversed: Map[String, Int],
                      filter: List[String]
                    ): (Int, List[String]) = {
    states match
      case state :: xs =>

        val nextStates = myMovements(state, filter)

        val filteredNextStates = nextStates.filterNot { s =>
          traversed.get(s.signature).exists(_ > s.releasedPressure)
        }

        val newTraversed = traversed.concat(filteredNextStates.map { s =>
          s.signature -> s.releasedPressure
        })

        val updatedStates = xs.filterNot { s =>
          newTraversed.get(s.signature).exists(_ > s.releasedPressure)
        } ++ filteredNextStates

        findMyMaxScore(
          updatedStates,
          newTraversed,
          filter
        )

      case _ =>
        val maxValue = traversed.values.max
        val traversedValves = traversed.find(_._2 == maxValue).get._1.drop(3).split('.').toList
        maxValue -> traversedValves
  }

  // Part 1

  println(findMyMaxScore(List(MyTrajectory(openedValves = Map.empty, maxTime = 30)), Map.empty, Nil)._1)

  // Part 2

  val (score, openedValves) = findMyMaxScore(List(MyTrajectory(openedValves = Map.empty, maxTime = 26)), Map.empty, Nil)
  val (score2, _) = findMyMaxScore(List(MyTrajectory(openedValves = Map.empty, maxTime = 26)), Map.empty, openedValves)
  println(score + score2)
}