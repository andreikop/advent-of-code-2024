import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day06/"

val lines = readFile(
  path + "input-short.txt"
)

val L = '⇠'
val R = '⇢'
val U = '⇡'
val D = '⇣'
val X = 'X'
val Empty = '.'
val Block = '#'

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

case class Field(arr: Array[Array[Char]], var g: Guard) {
  val xSize = arr.length
  val ySize = arr(0).length

  def at(x: Int, y: Int) = arr(x)(y)
  def at(p: Pos) = arr(p.x)(p.y)

  def set(pos: Pos, ch: Char) = arr(pos.x)(pos.y) = ch

  def print = {
    println("-" * (xSize + 2))
    for (yIndex <- (ySize - 1) to 0 by -1) {
      val line =
        for xIndex <- 0 until xSize
        yield if (Pos(xIndex, yIndex) != g.pos) arr(xIndex)(yIndex) else g.dir
      println("|" + line.mkString + "|")
    }
    println("-" * (xSize + 2))
    println()
  }

  def step = {
    at(g.nextPos) match {
      case Empty | X | L | R | U | D => g = g.move
      case Block                     => g = g.turn
    }
    set(g.pos, g.dirLetter)
  }

  def done: Boolean =
    g.nextPos.x < 0 || g.nextPos.x >= xSize || g.nextPos.y < 0 | g.nextPos.y >= ySize

  def res = arr.flatten.count(Array(L, R, U, D).contains(_))

  set(g.pos, g.dirLetter)
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
  Field(arr, g)
}
val f = makeField(lines)

f.print
while (!f.done) {
  f.step
  f.print
}

val res1 = f.res

//while (!f.done) {
//  val fClone = f.clone
//  f.step
//  // f.print
//}
