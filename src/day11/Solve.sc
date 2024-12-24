import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024-2/src/day11/"

val stones = readInts(
  path + "input-long.txt"
)(0).toList

def transform(stone: Long): List[Long] = stone match {
  case 0 => List(1)
  case _ if (stone.toString.length % 2 == 0) => {
    val str = stone.toString
    List(
      str.slice(0, str.length / 2).toInt,
      str.slice(str.length / 2, str.length).toInt)
  }
  case _ => List(stone * 2024)
}

def blink(stones: List[Long]): List[Long] =
  stones.map(transform).flatten


var st = stones.map(_.toLong)
var stoneMap = st.map((_, 1L)).toMap  // value is a key

for (step <- 1 to 75) {
  var newMap: Map[Long, Long] = Map().withDefault(_ => 0L)
  stoneMap.map((stone, stoneCount) => {
    for (newStone <- transform(stone))
    do newMap = newMap.updated(newStone, newMap(newStone) + stoneCount)
  }
  )
  stoneMap = newMap
  //st = blink(st)
}
st.length
stoneMap.values.sum
