package day06

def readFile(name: String): Seq[String] = {
  val f = Source.fromFile(name)
  val lines = f.getLines().toArray
  f.close
  lines
}

import scala.collection.immutable.AbstractSeq

import scala.io.Source

def printLine(vals: Any*) = {
  def str(v: Any) = {
    v match {
      case s: Array[Any]       => "[" + s.iterator.mkString(", ") + "]"
      case s: Array[Int]       => "[" + s.iterator.mkString(", ") + "]"
      case s: AbstractSeq[Any] => "[" + s.iterator.mkString(", ") + "]"
      case s: AbstractSeq[Int] => "[" + s.iterator.mkString(", ") + "]"
      case _                   => v.toString
    }
  }

  println(vals.map(str).mkString(" "))
}

object Day6App {
  def main(args: Array[String]): Unit = {
    val fName = args(0)
    val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day06/"

    val lines = readFile(
      path + fName
    )

    val L = '⇠'
    val R = '⇢'
    val U = '⇡'
    val D = '⇣'
    val X = 'X'
    val Empty = '.'
    val Block = '#'
    val Obstacle = 'O'

    case class Pos(x: Int, y: Int)

    case class Guard(pos: Pos, dir: Char) {
      val xDir = dir match {
        case '<' => -1
        case '>' => 1
        case _   => 0
      }
      val yDir = dir match {
        case 'v' => -1
        case '^' => 1
        case _   => 0
      }

      def nextPos = Pos(pos.x + xDir, pos.y + yDir)

      def nextDir: Char = dir match {
        case '>' => 'v'
        case 'v' => '<'
        case '<' => '^'
        case '^' => '>'
      }

      def dirLetter: Char = dir match {
        case '>' => R
        case 'v' => D
        case '<' => L
        case '^' => U
      }

      def move = Guard(nextPos, dir)

      def turn = Guard(pos, nextDir)
    }

    case class Field(arr: Array[Array[Char]], visited: List[Guard]) {
      assert(visited.length <= 10000)
      val g = visited.last
      val xSize = arr.length
      val ySize = arr(0).length

      def at(x: Int, y: Int) = arr(x)(y)

      def at(p: Pos) = arr(p.x)(p.y)

      def print = {
        val resArr = arr.map(_.clone)
        for (g <- visited)
          if (resArr(g.pos.x)(g.pos.y) != Obstacle)
            resArr(g.pos.x)(g.pos.y) = g.dirLetter

        println("-" * (xSize + 2))
        for (yIndex <- (ySize - 1) to 0 by -1) {
          val line =
            for xIndex <- 0 until xSize
            yield {
              val pos = Pos(xIndex, yIndex)
              if (pos == g.pos)
                g.dir
              else
                resArr(pos.x)(pos.y)
            }
          println("|" + line.mkString + "|")
        }
        println("-" * (xSize + 2))
        println()
      }

      def step: Field = {
        val nextG = at(g.nextPos) match {
          case Empty            => g.move
          case Block | Obstacle => g.turn
        }

        val f = Field(arr, visited :+ nextG)
        f
      }

      def done: Boolean =
        g.nextPos.x < 0 || g.nextPos.x >= xSize || g.nextPos.y < 0 | g.nextPos.y >= ySize

      def res = visited.map(_.pos).toSet.size

      def res2 = arr.flatten.count(_ == Obstacle)

      def foundLoop = visited.count(_ == g) > 1

      def solveUntilEnd: Field = {
        if (foundLoop) {
          // print
          this
        } else if (done) this
        else
          // println("\u001b[2J")
          // print
          // Thread.sleep(200)
          this.step.solveUntilEnd
      }

      def set(pos: Pos, ch: Char) = arr(pos.x)(pos.y) = ch
    }

    def makeField(lines: Seq[String]): Field = {
      val xSize = lines(0).length
      val ySize = lines.length

      val arr: Array[Array[Char]] = Array.ofDim[Char](xSize, ySize)
      var g: Guard = null

      for ((line, lineIndex) <- lines.zipWithIndex) {
        for ((char, charIndex) <- line.zipWithIndex) {
          val xIndex = charIndex
          val yIndex = ySize - lineIndex - 1
          char match {
            case Empty => arr(xIndex)(yIndex) = '.'
            case '<' | '>' | '^' | 'v' => {
              arr(xIndex)(yIndex) = '.'
              g = Guard(Pos(xIndex, yIndex), char)
            }
            case Block => arr(xIndex)(yIndex) = char
          }
        }
      }
      Field(arr, List(g))
    }

    val origF = makeField(lines)

    origF.print

    val f = origF.solveUntilEnd

    val res1 = f.res
    printLine("result 1", res1)
    // f.print

    var obstacles = Set[Pos]()

    val visited = f.visited.toArray
    for (visitedIndex <- 0 until (visited.length - 1)) {
      val guard = visited(visitedIndex)
      val nextGuard = visited(visitedIndex + 1)
      if (guard.pos != nextGuard.pos) { // only if going to move
        printLine("~ checking index", visitedIndex, "of", visited.length)
        val newVisited = visited.slice(0, visitedIndex + 1)
        if (!newVisited.map(_.pos).contains(nextGuard.pos)) {
          f.set(nextGuard.pos, Obstacle)
          val newField = Field(f.arr, newVisited.toList)
          val solved = newField.solveUntilEnd
          if (solved.foundLoop) {
            obstacles = obstacles.incl(nextGuard.pos)
            printLine(">> found loop at", guard.pos)
            // f.print
          }

          f.set(nextGuard.pos, Empty)
        }
      }
    }

    println("~~~~~ final result")
    printLine(obstacles.size)

  }
}
