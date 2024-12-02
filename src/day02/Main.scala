package day02

import aoc.input._
import aoc.print._

object Main {
  def isSafeP1(s: Array[Int]): Boolean = {
    val first = s.slice(0, s.length - 1)
    val second = s.slice(1, s.length)
    val difs = first.zip(second).map((f, s) => f - s)

    if (difs.forall(v => v > 0 && v <= 3))
      true
    else if (difs.forall(v => v < 0 && v >= -3))
      true
    else
      false
  }

  def withoutElement(s: Array[Int], index: Int): Array[Int] =
    s.slice(0, index) ++ s.slice(index + 1, s.length)

  def tryRemove(s: Array[Int], index: Int): Boolean = {
    val newS = withoutElement(s, index)
    isSafeP1(newS)
  }

  def isSafeP2(s: Array[Int]): Boolean = {
    val first = s.slice(0, s.length - 1)
    val second = s.slice(1, s.length)
    val difs = first.zip(second).map((f, s) => f - s)

    val goodPos = difs.map(v => v > 0 && v <= 3)
    val goodPosCount = goodPos.count(v => v)
    val goodNeg = difs.map(v => v < 0 && v >= -3)
    val goodNegCount = goodNeg.count(v => v)

    if (goodPosCount == difs.length || goodNegCount == difs.length)
      return true

    val badNegIndex = goodNeg.indexOf(false)
    if (badNegIndex >= 0) {
      if (tryRemove(s, badNegIndex) || tryRemove(s, badNegIndex + 1))
        return true
    }

    val badPosIndex = goodPos.indexOf(false)
    if (badPosIndex >= 0) {
      if (tryRemove(s, badPosIndex) || tryRemove(s, badPosIndex + 1))
        return true
    }

    false
  }

  def solve(fileName: String, f: Array[Int] => Boolean): Int = {
    val arrs = readInts("src/day02/" + fileName)

    arrs.count(f)
  }

  def main(args: Array[String]) = {
    printLine(s"part 1 short ${solve("input-short.txt", isSafeP1)}")
    printLine(s"part 1 long ${solve("input-long.txt", isSafeP1)}")

    printLine(s"part 2 short ${solve("input-short.txt", isSafeP2)}")
    printLine(s"part 2 long ${solve("input-long.txt", isSafeP2)}")
  }

}
