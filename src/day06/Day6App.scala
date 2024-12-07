package day06

def readFile(name: String): Seq[String] = {
  val f = Source.fromFile(name)
  val lines = f.getLines().toArray
  f.close
  lines
}

import scala.annotation.tailrec
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
    val Obstacle = 'o'
    val FoundObstacle = 'O'

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

    case class Field(
        arr: Array[Array[Char]],
        visitedSet: Set[Guard],
        g: Guard
    ) {
      val xSize = arr.length
      val ySize = arr(0).length

      def at(x: Int, y: Int) = arr(x)(y)

      def at(p: Pos) = arr(p.x)(p.y)

      def print = {
        val resArr = arr.map(_.clone)
        for (g <- visitedSet)
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

      def nextGuard(g: Guard): Guard = at(g.nextPos) match {
        case Empty | FoundObstacle => g.move
        case Block | Obstacle      => g.turn
      }

      def done(gToCheck: Guard): Boolean = {
        val p = gToCheck.nextPos
        p.x < 0 || p.x >= xSize || p.y < 0 | p.y >= ySize
      }

      def res = visitedSet.map(_.pos).size

      def res2 = arr.flatten.count(_ == FoundObstacle)

      def foundLoop = visitedSet.contains(g)

      def solveUntilEnd: Field = solveUntilEndReq(visitedSet, g)

      @tailrec
      private def solveUntilEndReq(visitedSet: Set[Guard], g: Guard): Field = {
        // print
        if (visitedSet.contains(g)) {
          // print
          Field(arr, visitedSet, g)
        } else if (done(g)) Field(arr, visitedSet, g)
        else
          // println("\u001b[2J")
          // print
          // Thread.sleep(200)
          solveUntilEndReq(visitedSet.incl(g), nextGuard(g))
      }

      def solveUntilEndPart2: Field = solveUntilEndPart2Req(visitedSet, g)

      @tailrec
      private def solveUntilEndPart2Req(
          visitedSet: Set[Guard],
          g: Guard
      ): Field = {
        if (visitedSet.contains(g)) { // found loop
          // print
          this
        } else if (done(g)) this
        else {
          // println("\u001b[2J")
          // print
          // Thread.sleep(200)
          val nextG = nextGuard(g)
          if (
            nextG.pos != g.pos &&
            at(nextG.pos) == Empty &&
            !visitedSet.map(_.pos).contains(nextG.pos)
          ) {
            // printLine("~ checking pos", g.pos)
            set(nextG.pos, Obstacle)
            val solved = solveUntilEnd
            if (solved.foundLoop) {
              // obstacles = obstacles.incl(nextG.pos)
              // printLine(">> found loop at", g.pos)
              // f.print
              set(nextG.pos, FoundObstacle)
            } else {
              set(nextG.pos, Empty)
            }
          }
          solveUntilEndPart2Req(visitedSet.incl(g), nextG)
        }
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
      Field(arr, Set(), g)
    }

    val origF = makeField(lines)

    origF.print

    val f = origF.solveUntilEnd

    val res1 = f.res
    printLine("result 1", res1)
    // f.print

    printLine("result 2", origF.solveUntilEndPart2.res2)
  }
}
