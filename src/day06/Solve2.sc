import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day05/"

val lines = readFile(
  path + "input-long.txt"
)

case class Pair(a: Int, b: Int)

var foundBlank = false
var pairs: Map[Int, Set[Int]] = Map().withDefault(x => Set[Int]())
var updates = List[Array[Int]]()

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
      updates = updates :+ parts.map(_.toInt)
    }
  }
}

//printLine(pairs)
//printLine(updates)

var res = 0

def fixUpdate(u: Array[Int]): Boolean = {
  var wasWalid = true

  for (elemIndex <- 1 until u.length) {
    var elem = u(elemIndex)
    val sliceBefore = u.slice(0, elemIndex)
    val nextSet = pairs(elem)
    for (elemBeforeIndex <- 0 until sliceBefore.length) {
      val elemBefore = sliceBefore(elemBeforeIndex)
      if (nextSet(elemBefore)) {
        u.update(elemIndex, elemBefore)
        u.update(elemBeforeIndex, elem)
        elem = elemBefore
        wasWalid = false
      }
    }
  }

  wasWalid
}

//val resItems = for
//  u <- updates
//  if fixUpdate(u)
//yield u(u.length / 2)
//
//printLine(resItems.sum())

var res2 = 0
for (up <- updates) {
  printLine("Before fix", up)
  val wasValid = fixUpdate(up)
  if (!wasValid) {
    printLine("fixed", up)
    res2 += up(up.length / 2)
  }
}

printLine(res2)
