import Util.readFile

import scala.util.matching.Regex

@main def day21(): Unit = {

  val p0: Regex = """(.{4}): (.*)""".r
  val p1: Regex = """(.{4}) (.) (.{4})""".r

  sealed trait Expression
  case class Value(i: BigInt) extends Expression
  case class Operation(a: String, b: String, op: Char) extends Expression

  def parse(ex: String): Expression = ex match {
    case p1(a, op, b) => Operation(a, b, op.head)
    case _ => Value(BigInt(ex))
  }

  val input = readFile("resources/day21").map {
    case p0(id, expression) =>
      id -> parse(expression)
  }.toMap

  def operate(op: Char, x: BigInt, y: BigInt): BigInt = op match {
    case '+' => x + y
    case '-' => x - y
    case '/' => x / y
    case '*' => x * y
  }

  def eval(ex: Expression, expressions: Map[String, Expression]): BigInt = ex match {
    case Value(i) => i
    case Operation(a, b, op) => operate(op, eval(expressions(a), expressions), eval(expressions(b), expressions))
  }

  // Part 1

  println(eval(input("root"), input))

  // Part 2

  def reverse(op: Char): Char = op match {
    case '+' => '-'
    case '-' => '+'
    case '/' => '*'
    case '*' => '/'
  }

  def find(key: String, input: Map[String, Expression]): BigInt = {
    // Is it a defined as an expression
    input.get(key) match {
      case Some(expression) =>

        val newInput = input - key

        expression match {
          case Value(i) => i
          case Operation(a, b, op) => operate(op, find(a, newInput), find(b, newInput))
        }

      case _ =>
        // It must be an argument of an expression
        val (key2, operation) = input.find { case (k, v) =>
          v match {
            case Operation(a, b, _) if a == key || b == key => true
            case _ => false
          }
        }.map { (k, v) =>
          k -> v.asInstanceOf[Operation]
        }.get

        val newInput = input - key2

        key2 match {
          case "root" if operation.a == key =>
            find(operation.b, newInput)

          case "root" =>
            find(operation.a, newInput)

          case _ if operation.a == key =>
            operate(reverse(operation.op), find(key2, newInput), find(operation.b, newInput))

          case _ if operation.op == '-' || operation.op == '/' =>
            operate(operation.op, find(operation.a, newInput), find(key2, newInput))

          case _ =>
            operate(reverse(operation.op), find(key2, newInput), find(operation.a, newInput))
        }
    }
  }

  println(find("humn", input - "humn"))
}