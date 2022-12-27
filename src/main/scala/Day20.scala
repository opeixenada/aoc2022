import Util.readFile

@main def day20(): Unit = {

  val input = readFile("resources/day20").map(BigInt(_)).zipWithIndex

  // Part 1

  def move(xs: List[(BigInt, Int)], i: Int) = {
    val n = xs.length
    val ((value, _), position) = xs.zipWithIndex.find(_._1._2 == i).get

    val newPosition = {
      val x = ((value + (value.abs / (n - 1) + 1) * (n - 1)) % (n - 1) + position) % (n - 1)
      if (x == 0) n - 1 else x.toInt
    }

    val filtered = xs.filterNot(_._2 == i)
    filtered.take(newPosition) ::: ((value, i) :: filtered.drop(newPosition))
  }

  def grove(xs: Seq[(BigInt, Int)]) = {
    val zeroIndex = xs.indexWhere(_._1 == 0)

    List(1000, 2000, 3000)
      .map { x => xs((zeroIndex + x) % input.length)._1 }
      .sum
  }

  println(grove(
    input.indices.foldLeft(input)(move).toIndexedSeq
  ))

  // Part 2

  val key = BigInt(811589153)

  println(grove(
    List.fill(10)(input.indices.toList).flatten
      .foldLeft(input.map { case (x, i) => (x * key, i) })(move).toIndexedSeq
  ))
}