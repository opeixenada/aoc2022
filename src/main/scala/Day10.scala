import Util.readFile
import sun.security.util.Length

@main def day10(): Unit = {

  val input = readFile("resources/day10")

  def isInterestingCycle(x: Int) = (x - 20) % 40 == 0

  // Part 1
  // Part 2

  val (_, interestingSum, _) = input.flatMap {
    case "noop" => List(0)
    case x => List(0, x.drop(5).toInt)
  }
    .foldLeft((1, 0, 1)) { case ((x, sum, cycle), plus) =>
      if ((x - (cycle - 1) % 40).abs < 2) print('#') else print('.')
      if (cycle % 40 == 0) print('\n')

      (x + plus, sum + (if (isInterestingCycle(cycle)) x * cycle else 0), cycle + 1)
    }

  println()
  println(interestingSum)
}

