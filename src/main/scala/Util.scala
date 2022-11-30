import scala.io.Source

object Util {
  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val result = bufferedSource.getLines.toList
    bufferedSource.close
    result
  }
}
