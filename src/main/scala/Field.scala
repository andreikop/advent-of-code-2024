package aoc

import scala.reflect.ClassTag

case class Pos(x: Int, y: Int) {
  def left = Pos(x - 1, y)
  def right = Pos(x + 1, y)
  def up = Pos(x, y - 1)
  def down = Pos(x, y + 1)
}

case class Cell[T](
    field: Field[T],
    pos: Pos
) {
  def x = pos.x
  def y = pos.y

  def value: T = field.get(pos)
  def validNeighbours: Seq[Cell[T]] = field.validNeighbours(this.pos)
  def allNeighbourPos: Seq[Pos] = field.allNeighbourPos(this.pos)

  def isVisited: Boolean = field.isVisited(pos)
  def markVisited = field.markVisited(pos)

  override def toString = s"'$value'($x, $y)"
}

def readField[T: ClassTag](
    filePath: String,
    chConverter: (Char => T)
): Field[T] = {
  val lines: Array[String] = readFile(filePath)
  val arrays: Array[Array[T]] =
    lines.map(line => line.toCharArray.map(chConverter))

  Field[T](arrays)
}

def readCharField(filePath: String): Field[Char] =
  readField[Char](filePath, ch => ch)

case class Field[T](arr: Array[Array[T]]) {
  val width: Int = arr(0).length
  val height: Int = arr.length

  private val visitedFlags =
    Array.ofDim[Boolean](width, height) // flag if the field is visited

  def get(p: Pos): T = arr(p.y)(p.x)
  def get(x: Int, y: Int): T = arr(y)(x)

  def validPos(p: Pos) =
    (p.y >= 0 && p.y < height && p.x >= 0 && p.x < width)

  def getOption(p: Pos): Option[T] =
    if (validPos(p))
      Some(arr(p.y)(p.x))
    else
      None

  def cells: Seq[Cell[T]] = for (
    y <- 0 until height;
    x <- 0 until width
  ) yield Cell(this, Pos(x, y))

  def allNeighbourPos(pos: Pos): Seq[Pos] =
    Seq(pos.left, pos.right, pos.up, pos.down)

  def validNeighbours(pos: Pos): List[Cell[T]] = {
    List(pos.left, pos.right, pos.up, pos.down)
      .filter(validPos(_))
      .map(pos => Cell(this, pos))
  }

  def isVisited(pos: Pos): Boolean = visitedFlags(pos.y)(pos.x)
  def markVisited(pos: Pos) = visitedFlags(pos.y)(pos.x) = true

  override def toString: String =
    s"${width}x${height}\n" + arr.map(_.mkString).mkString("\n")

}

type ChCell = Cell[Char]
type ChField = Field[Char]
