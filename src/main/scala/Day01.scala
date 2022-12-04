import Util.readFile

@main def day01(): Unit = {

  val input = readFile("resources/day01")

  // Part 1

  val (maxCalories, _) = input.foldLeft((0, 0)) {
    case ((currentMax, currentSum), s) =>
      if (s.isBlank) (Math.max(currentSum, currentMax), 0)
      else (currentMax, currentSum + s.toInt)
  }

  println(maxCalories)

  // Part 2

  val weights = input.foldLeft(List(0)) {
    case (x :: xs, s) =>
      if (s.isBlank) 0 :: x :: xs
      else (x + s.toInt) :: xs
  }

  println(weights.sorted.takeRight(3).sum)
}