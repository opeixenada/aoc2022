import Util.readFile

  import scala.util.matching.Regex

@main def day07(): Unit = {

  val input = readFile("resources/day07")

  val cdPattern: Regex = """\$ cd (.+)""".r
  val filePattern: Regex = """(\d+) (.+)""".r

  def parent(path: String) = path.split('/').dropRight(1).mkString("/")

  val (_, filesMap) = input.foldLeft("", Map.empty[String, BigInt]) {
    case ((currentDirectory, files), command) =>
      command match
        case cdPattern(path) => path match
          case "/" => ("", files)
          case ".." => (parent(currentDirectory), files)
          case _ => (s"$currentDirectory/$path", files)

        case filePattern(size, path) => (currentDirectory, files + (s"$currentDirectory/$path" -> BigInt(size)))

        case _ => (currentDirectory, files)
  }

  def parents(path: String) = path.split('/').tail.dropRight(1).foldLeft(List("")) {
    case (x :: xs, component) => s"$x/$component" :: x :: xs
  }

  val directoriesSizes = filesMap.foldLeft(Map.empty[String, BigInt]) {
    case (acc, (path, size)) => parents(path).foldLeft(acc) {
      case (acc2, dirPath) => acc2 + (dirPath -> (acc2.getOrElse(dirPath, BigInt(0)) + size))
    }
  }

  // Part 1

  println(directoriesSizes.values.filter(_ < BigInt(100000)).sum)

  // Part 2

  println(directoriesSizes.values.filter(_ > (BigInt(30000000) - BigInt(70000000) + directoriesSizes(""))).min)
}

