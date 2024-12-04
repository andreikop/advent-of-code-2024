import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day04/"

val lines = readFile(
  path + "input-long-2.txt"
)

val xSize = lines(0).length
val ySize = lines.length

def at(x: Int, y: Int): Char = lines(y).charAt(x)

def collectString(x: Int, y: Int): String = {
  val chars = for
    xDif <- List(-1, 1)
    yDif <- List(-1, 1)
  yield at(x + xDif, y + yDif)

  chars.mkString
}

def canStartHere(x: Int, y: Int): Boolean = {
  (x - 1) >= 0 && (x + 1) < xSize && (y - 1) >= 0 && (y + 1) < ySize
}

var count = 0

for
  x <- 0 until xSize
  y <- 0 until ySize
  if at(x, y) == 'A'
  if canStartHere(x, y)
do {
  val str = collectString(x, y)
  if (
    str.count(_ == 'M') == 2 &&
    str.count(_ == 'S') == 2 &&
    str.charAt(1) != str.charAt(2)
  )
    count += 1
}

println(count)
