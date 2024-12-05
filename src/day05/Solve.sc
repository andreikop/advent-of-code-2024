import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day05/"

val lines = readFile(
  path + "input-long.txt"
)

case class Pair(a: Int, b: Int)

var foundBlank = false
var pairs: Map[Int, Set[Int]] = Map().withDefault(x => Set[Int]())
var updates = List[List[Int]]()

for (line <- lines) {
  if (line.isBlank)
    foundBlank = true
  else {
    if (!foundBlank) {
      val parts = line.split("\\|").map(_.toInt)
      val a = parts(0)
      val b = parts(1)
      pairs = pairs.updated(a, pairs(a).incl(b))
    } else {
      val parts = line.split(",")
      updates = updates :+ parts.map(_.toInt).toList
    }
  }
}

printLine(pairs)
printLine(updates)

var res = 0

def goodUpdate(u: List[Int]): Boolean = {
  var res = true

  for (i <- 1 until u.length) {
    val elem = u(i)
    val sliceBefore = u.slice(0, i)
    printLine("For element", elem, "slice before", sliceBefore)
    val nextSet = pairs(elem)
    printLine("will check next set", nextSet)
    for (elemBefore <- sliceBefore) {
      if (nextSet(elemBefore))
        res = false
    }
  }

  res
}

val resItems = for
  u <- updates
  if goodUpdate(u)
yield u(u.length / 2)

printLine(resItems.sum())
