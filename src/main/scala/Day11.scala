import Util.readFile

import scala.annotation.tailrec
import scala.util.matching.Regex

@main def day11(): Unit = {

  val input = readFile("resources/day11")

  val operationPattern: Regex = """  Operation: new = (.+) (.) (.+)""".r

  case class Operation(first: String, second: String, operation: Char) {
    def eval(old: Int): Int = {
      val firstValue = getValue(first, old)
      val secondValue = getValue(second, old)

      operation match
        case '*' => firstValue * secondValue
        case '+' => firstValue + secondValue
    }

    private def getValue(s: String, old: Int): Int = s match
      case "old" => old
      case x => x.toInt
  }

  def add[T](item: T, index: Int, throwMap: Map[Int, List[T]]): Map[Int, List[T]] =
    throwMap + (index -> throwMap.getOrElse(index, Nil).appended(item))

  case class Monkey(
                     id: Int,
                     items: List[Int],
                     operation: Operation,
                     test: Int,
                     ifTrue: Int,
                     ifFalse: Int,
                     inspected: Int
                   ) {
    def updateThrowMap(
                        throwMap: Map[Int, List[Int]],
                        item: Int
                      ): Map[Int, List[Int]] = {
      val newWorryLevel = operation.eval(item) / 3
      add(newWorryLevel, if (newWorryLevel % test == 0) ifTrue else ifFalse, throwMap)
    }
  }

  @tailrec
  def parse(xs: List[String], monkeys: List[Monkey]): List[Monkey] = {
    xs match
      case a :: b :: c :: d :: e :: f :: _ =>
        val id = a.drop("Monkey ".length).dropRight(1).toInt

        val startingItems = b.drop("  Starting items: ".length).split(", ").map(_.toInt).toList

        val operation = c match
          case operationPattern(first, operation, second) => Operation(first, second, operation.head)

        val test = d.drop("  Test: divisible by ".length).toInt
        val ifTrue = e.drop("    If true: throw to monkey ".length).toInt
        val ifFalse = f.drop("    If false: throw to monkey ".length).toInt

        val monkey = Monkey(id, startingItems, operation, test, ifTrue, ifFalse, 0)

        parse(xs.drop(7), monkeys.appended(monkey))
      case _ => monkeys
  }

  val parsedInput = parse(input, Nil)

  // Part 1

  def part1(monkeys: List[Monkey]): Int = {

    @tailrec
    def throwItems(
                    doneMonkeys: List[Monkey],
                    leftMonkeys: List[Monkey],
                    additionalItems: Map[Int, List[Int]]
                  ): List[Monkey] = {
      leftMonkeys match
        case Nil =>
          doneMonkeys.zipWithIndex.map { (m, i) =>
            m.copy(items = m.items.appendedAll(additionalItems.getOrElse(i, Nil)))
          }

        case m :: ms =>
          val toInspect = m.items
            .appendedAll(additionalItems.getOrElse(m.id, Nil))

          val newAdditionalItems = toInspect
            .foldLeft(additionalItems)((x, y) => m.updateThrowMap(x, y))

          val newMonkey = m.copy(items = Nil, inspected = m.inspected + toInspect.size)

          throwItems(doneMonkeys.appended(newMonkey), ms, newAdditionalItems - m.id)
    }

    (0 until 20).foldLeft(monkeys) { (monkeys, _) =>
      throwItems(Nil, monkeys, Map.empty)
    }
      .map(_.inspected).sorted.takeRight(2).product
  }

  println(part1(parsedInput))

  // Part 2

  def part2(monkeys: List[Monkey]): BigInt = {

    type Remainders = Map[Int, Int]

    val primes = input.filter(_.startsWith("  Test: divisible by ")).map(_.drop("  Test: divisible by ".length).toInt)

    case class Monkey2(
                        id: Int,
                        items: List[Remainders],
                        operation: Operation,
                        test: Int,
                        ifTrue: Int,
                        ifFalse: Int,
                        inspected: Int
                      ) {
      def updateThrowMap(
                          throwMap: Map[Int, List[Remainders]],
                          item: Remainders,
                        ): Map[Int, List[Remainders]] = {
        val newWorryLevel: Remainders = item.map { (k, v) =>
          k -> (operation.eval(v) % k)
        }
        add(newWorryLevel, if (newWorryLevel.get(test).contains(0)) ifTrue else ifFalse, throwMap)
      }
    }

    val monkeys2 = monkeys.map { m =>
      Monkey2(m.id, m.items.map { x =>
        primes.map { k =>
          k -> (x % k)
        }.toMap
      }, m.operation, m.test, m.ifTrue, m.ifFalse, m.inspected)
    }

    @tailrec
    def throwItems(
                    doneMonkeys: List[Monkey2],
                    leftMonkeys: List[Monkey2],
                    additionalItems: Map[Int, List[Remainders]]
                  ): List[Monkey2] = {
      leftMonkeys match
        case Nil =>
          doneMonkeys.zipWithIndex.map { (m, i) =>
            m.copy(items = m.items.appendedAll(additionalItems.getOrElse(i, Nil)))
          }

        case m :: ms =>
          val toInspect = m.items
            .appendedAll(additionalItems.getOrElse(m.id, Nil))

          val newAdditionalItems = toInspect
            .foldLeft(additionalItems)((x, y) => m.updateThrowMap(x, y))

          val newMonkey = m.copy(items = Nil, inspected = m.inspected + toInspect.size)

          throwItems(doneMonkeys.appended(newMonkey), ms, newAdditionalItems - m.id)
    }

    (0 until 10000).foldLeft(monkeys2) { (monkeys, _) =>
      throwItems(Nil, monkeys, Map.empty)
    }.map(_.inspected).sorted.takeRight(2).map(BigInt(_)).product
  }

  println(part2(parsedInput))
}