import Util.readFile

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.matching.Regex

@main def day19(): Unit = {

  val pattern: Regex = """Blueprint (\d*): Each ore robot costs (\d*) ore. Each clay robot costs (\d*) ore. Each obsidian robot costs (\d*) ore and (\d*) clay. Each geode robot costs (\d*) ore and (\d*) obsidian.""".r

  case class Blueprint(
                        id: Int,
                        oreRobotOre: Int,
                        clayRobotOre: Int,
                        obsidianRobotOre: Int,
                        obsidianRobotClay: Int,
                        geodeRobotOre: Int,
                        geodeRobotObsidian: Int
                      )

  val input = readFile("resources/day19").map {
    case pattern(id, oreRobotOre, clayRobotOre, obsidianRobotOre, obsidianRobotClay, geodeRobotOre, geodeRobotObsidian) =>
      Blueprint(
        id.toInt,
        oreRobotOre.toInt,
        clayRobotOre.toInt,
        obsidianRobotOre.toInt,
        obsidianRobotClay.toInt,
        geodeRobotOre.toInt,
        geodeRobotObsidian.toInt
      )
  }

  // Part 1

  case class State(
                    bp: Blueprint,
                    oreRobots: List[Int] = List(0),
                    clayRobots: List[Int] = Nil,
                    obsidianRobots: List[Int] = Nil,
                    geodeRobots: List[Int] = Nil,
                  ) {

    override def toString: String = {
      s"[$minute] R(Ore): ${oreRobots.mkString(",")} | R(Clay): ${clayRobots.mkString(",")} | R(Obs): ${obsidianRobots.mkString(",")} | R(G): ${geodeRobots.mkString(",")}"
    }

    val minute: Int = (oreRobots ::: clayRobots ::: obsidianRobots ::: geodeRobots).max

    def potentialGeodes(t: Int): Int = (geodeRobots ::: (minute + 1).until(t).toList).filter(_ < t).map(t - _).sum

    def geodes(t: Int): Int = geodeRobots.filter(_ < t).map(t - _).sum

    def ore(t: Int): Int = oreRobots.filter(_ < t).map(t - _).sum
      - (oreRobots.count(_ <= t) - 1) * bp.oreRobotOre
      - clayRobots.count(_ <= t) * bp.clayRobotOre
      - obsidianRobots.count(_ <= t) * bp.obsidianRobotOre
      - geodeRobots.count(_ <= t) * bp.geodeRobotOre

    def clay(t: Int): Int = clayRobots.filter(_ < t).map(t - _).sum
      - obsidianRobots.count(_ <= t) * bp.obsidianRobotClay

    def obsidian(t: Int): Int = obsidianRobots.filter(_ < t).map(t - _).sum
      - geodeRobots.count(_ <= t) * bp.geodeRobotObsidian

  }

  def next(s: State, t: Int): List[State] = {

    // Next: ore robot
    val stepsUntilOreRobot = Math.max(((s.bp.oreRobotOre - s.ore(s.minute)).toFloat / s.oreRobots.size).ceil.toInt + 1, 1)
    val s1 =
      if (s.minute + stepsUntilOreRobot > t - 2) None
      else Some(s.copy(oreRobots = (s.minute + stepsUntilOreRobot) :: s.oreRobots))

    // Next: clay robot
    val stepsUntilClayRobot = Math.max(((s.bp.clayRobotOre - s.ore(s.minute)).toFloat / s.oreRobots.size).ceil.toInt + 1, 1)
    val s2 =
      if (s.minute + stepsUntilClayRobot > t - 3) None
      else Some(s.copy(clayRobots = (s.minute + stepsUntilClayRobot) :: s.clayRobots))

    // Next: obsidian robot
    val s3 =
      if (s.clayRobots.isEmpty) None
      else {
        val stepsUntilObsidianRobotOre = ((s.bp.obsidianRobotOre - s.ore(s.minute)).toFloat / s.oreRobots.size).ceil.toInt + 1
        val stepsUntilObsidianRobotClay = ((s.bp.obsidianRobotClay - s.clay(s.minute)).toFloat / s.clayRobots.size).ceil.toInt + 1
        val stepsUntilObsidianRobot = List(1, stepsUntilObsidianRobotOre, stepsUntilObsidianRobotClay).max
        if (s.minute + stepsUntilObsidianRobot > t - 2) None
        else Some(s.copy(obsidianRobots = (s.minute + stepsUntilObsidianRobot) :: s.obsidianRobots))
      }

    // Next: geodes robot
    val s4 =
      if (s.obsidianRobots.isEmpty) None
      else {
        val stepsUntilGeodesRobotOre = ((s.bp.geodeRobotOre - s.ore(s.minute)).toFloat / s.oreRobots.size).ceil.toInt + 1
        val stepsUntilGeodesRobotObsidian = ((s.bp.geodeRobotObsidian - s.obsidian(s.minute)).toFloat / s.obsidianRobots.size).ceil.toInt + 1
        val stepsUntilGeodesRobot = List(1, stepsUntilGeodesRobotOre, stepsUntilGeodesRobotObsidian).max
        if (s.minute + stepsUntilGeodesRobot > t - 1) None
        else Some(s.copy(geodeRobots = (s.minute + stepsUntilGeodesRobot) :: s.geodeRobots))
      }

    List(s1, s2, s3, s4).flatten.reverse
  }

  @tailrec
  def traverse(states: List[State], maxGeodes: Int, t: Int): Int = {
    states match
      case s :: ss if s.potentialGeodes(t) > maxGeodes =>
        val nextStates = next(s, t)
        val newMax = nextStates.map(_.geodes(t)).maxOption.getOrElse(0)
        traverse(nextStates ::: ss, Math.max(maxGeodes, newMax), t)

      case _ :: ss =>
        traverse(ss, maxGeodes, t)

      case _ => maxGeodes
  }

  implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(30))

  val result = input.map { bp =>
    Future {
      traverse(List(State(bp)), 0, 24)
    }
  }

  println(Await.result(Future.sequence(result).map(_.sum), 1.minute))

  // Part 2

  val result2 = input.take(3).map { bp =>
    Future {
      traverse(List(State(bp)), 0, 32)
    }
  }

  println(Await.result(Future.sequence(result2).map(_.product), 5.minute))

  System.exit(0)
}
