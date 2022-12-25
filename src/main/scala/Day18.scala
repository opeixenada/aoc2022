import Util.readFile

@main def day18(): Unit = {

  val input: Set[(Int, Int, Int)] = readFile("resources/day18")
    .map(_.split(',').map(_.toInt).toIndexedSeq)
    .map { cs => (cs(0), cs(1), cs(2)) }
    .toSet

  def getSides(startCoordinates: Iterable[(Int, Int, Int)]): List[List[Set[(Int, Int, Int)]]] =
    startCoordinates.toList.map { cs =>
      val xs = Set(
        (cs(0), cs(1), cs(2)),
        (cs(0) + 1, cs(1), cs(2))
      )

      val ys = Set(
        (cs(0), cs(1), cs(2)),
        (cs(0), cs(1) + 1, cs(2))
      )

      val zs = Set(
        (cs(0), cs(1), cs(2)),
        (cs(0), cs(1), cs(2) + 1)
      )

      List(xs, ys, zs)
    }

  val allSides = getSides(input)

  // Part 1

  def xor[T](xs: Set[T], ys: Set[T]) = xs.diff(ys).union(ys.diff(xs))

  def xorSides(sides: List[List[Set[(Int, Int, Int)]]]) =
    sides.foldLeft(List.fill(3)(Set.empty[(Int, Int, Int)])) {
      case (acc, xs) => acc.zip(xs).map(xor(_, _))
    }

  val xoredSides = xorSides(allSides)

  val sidesCount = xoredSides.foldLeft(0) {
    case (acc, xs) => acc + xs.size
  }

  println(sidesCount)

  // Part 2

  val (xmin, xmax) = xoredSides.head.map(_._1).min -> xoredSides.head.map(_._1).max
  val (ymin, ymax) = xoredSides.head.map(_._2).min -> xoredSides.head.map(_._2).max
  val (zmin, zmax) = xoredSides.head.map(_._3).min -> xoredSides.head.map(_._3).max

  val emptyCs = for {
    x <- xmin to xmax
    y <- ymin to ymax
    z <- zmin to zmax
    if !input.contains((x, y, z))
  } yield (x, y, z)


  def belongs(space: Set[(Int, Int, Int)], x: Int, y: Int, z: Int): Boolean = space.exists {
    case (x2, y2, z2) => (x == x2 && y == y2 && (z - z2).abs == 1) ||
      (z == z2 && y == y2 && (x - x2).abs == 1) ||
      (x == x2 && z == z2 && (y - y2).abs == 1)
  }

  val spaces: List[Set[(Int, Int, Int)]] = emptyCs
    .foldLeft(List.empty[Set[(Int, Int, Int)]]) {
      case (emptySpaces, (x, y, z)) =>
        val (matching, nonMatching) = emptySpaces.partition { space => belongs(space, x, y, z) }
        val matchingMerged = matching.foldLeft(Set((x, y, z))) { case (mergedSpace, toMerge) =>
          mergedSpace.union(toMerge)
        }
        matchingMerged :: nonMatching
    }
    .filterNot {
      _.exists { case (x, y, z) =>
        x == xmin || x == xmax || y == ymin || y == ymax || z == zmin || z == zmax
      }
    }

  val negSidesCount = spaces
    .flatMap { space => xorSides(getSides(space)) }
    .foldLeft(0) { case (acc, xs) => acc + xs.size }

  println(sidesCount - negSidesCount)
}
