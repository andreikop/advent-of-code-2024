import aoc.input._
import aoc.print._

val lines = readFile(
  "/Users/ak/code/aoc-2024/aoc-2024/src/day03/" + "input-short.txt"
)

val re = "mul\\((\\d+),(\\d+)\\)".r

case class Mul(a: Int, b: Int) {
  def res = a * b
}

val ops1 = lines
  .map(s =>
    re.findAllMatchIn(s)
      .map(m => Mul(m.group(1).toInt, m.group(2).toInt))
      .toList
  )
  .flatten

val res = ops1.map(_.res).sum
print("part1", res)

val lines2Short = readFile(
  "/Users/ak/code/aoc-2024/aoc-2024/src/day03/" + "input-short2.txt"
)
val linesLong = readFile(
  "/Users/ak/code/aoc-2024/aoc-2024/src/day03/" + "input-long.txt"
)

case class Do()
case class Dont()
val re2 = "(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\))".r

val matches2 = linesLong.map(s => re2.findAllMatchIn(s)).toList.flatten

val ops2 = matches2
  .map(m =>
    m.group(0).substring(0, 3) match {
      case "mul" => Mul(m.group(2).toInt, m.group(3).toInt)
      case "do(" => Do()
      case "don" => Dont()
    }
  )

var isDo = true
var acc = 0
for (op <- ops2)
  op match {
    case m: Mul => if (isDo) acc += m.res
    case Do()   => isDo = true
    case Dont() => isDo = false
  }

printLine("res2", acc)
