import Util.readFile

@main def day02(): Unit = {

  val input = readFile("resources/day02")

  // Part 1

  val totalScore = input.foldLeft(0) {
    case (score, s) =>
      val a = s.head - 'A'
      val b = s.last - 'X'
      3 * ((b - a + 4) % 3) + (b + 1) + score
  }

  println(totalScore)

  // Part 2

  val totalScore2 = input.foldLeft(0) {
    case (score, s) =>
      (s.head match {
        case 'A' => s.last match {
          case 'X' => 3
          case 'Y' => 1
          case 'Z' => 2
        }
        case 'B' => s.last match {
          case 'X' => 1
          case 'Y' => 2
          case 'Z' => 3
        }
        case _ => s.last match {
          case 'X' => 2
          case 'Y' => 3
          case 'Z' => 1
        }
      }) + (s.last - 'X') * 3 + score
  }

  println(totalScore2)
}