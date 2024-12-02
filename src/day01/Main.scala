package day01

import aoc.input._
import aoc.print._

object Main {
  def main(args: Array[String]) = {
    val (a, b) = parseTwoInts(readFile("src/day01/input-long.txt"))

    val dif = a.sorted.zip(b.sorted).map(_ - _).map(_.abs)

    println("part 1 " + dif.sum)

    val p2 = a.map(v => v * b.count(_ == v)).sum
    print(p2)
  }

}
