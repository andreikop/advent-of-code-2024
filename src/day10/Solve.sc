import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024-2/src/day10/"

val nums = readIntsWoSpaces(
  path + "input-long.txt"
)

val xSize = nums(0).length
val ySize = nums.length

def get(x: Int, y: Int) =
  if (x < 0) -1
  else if (x >= xSize) -1
  else if (y < 0) -1
  else if (y >= ySize) -1
  else nums(y)(x)

def checkPosition(height: Int, x: Int, y: Int): List[(Int, Int)] = {
  if (height == 9)
    List((x, y))
  else {
    val found = for (
      dif <- Seq((-1, 0), (0, -1), (0, 1), (1, 0))
    )
    yield {
      val newX = x + dif._1
      val newY = y + dif._2
      if (get(newX, newY) == height + 1)
        checkPosition(height + 1, newX, newY)
      else
        List()
    }

    found.toList.flatten
  }
}

val resList = for (
  x <- 0 until xSize;
  y <- 0 until ySize;
  if get(x, y) == 0
)
yield checkPosition(0, x, y).toSet.size

resList.fold(0)(_ + _)
