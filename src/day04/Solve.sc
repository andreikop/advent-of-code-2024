import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day04/"

val lines = readFile(
  path + "input-long.txt"
)

val xSize = lines(0).length
val ySize = lines.length

def at(x: Int, y: Int): Char = lines(y).charAt(x)

def collectString(x: Int, y: Int, xDif: Int, yDif: Int): String = {
  val chars =
    for chIndex <- 0 until 4
    yield at(x + xDif * chIndex, y + yDif * chIndex)

  chars.mkString
}

def canStartHere(x: Int, y: Int, xDir: Int, yDir: Int): Boolean = {
  val newX = x + (3 * xDir)
  val newY = y + (3 * yDir)
  newX >= 0 && newX < xSize && newY >= 0 && newY < ySize
}

var count = 0

for
  x <- 0 until xSize
  y <- 0 until ySize
  xDir <- (-1) to 1
  yDir <- -1 to 1
  if (xDir, yDir) != (0, 0)
  if at(x, y) == 'X'
  if canStartHere(x, y, xDir, yDir)
  if collectString(x, y, xDir, yDir) == "XMAS"
do count += 1

println(count)
