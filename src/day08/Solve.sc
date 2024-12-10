import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day08/"

val lines = readFile(
  path + "input-long.txt"
)

val xSize = lines(0).length
val ySize = lines.length

case class Antenna(x: Int, y: Int, freq: Char)

case class AntiNode(x: Int, y: Int) {
  def valid = x >= 0 && x < xSize && y >= 0 && y < ySize
}

val antennas = lines.zipWithIndex
  .map((line, lineIndex) =>
    line.zipWithIndex.map((char, charIndex) =>
      Antenna(charIndex, lines.length - lineIndex - 1, char)
    )
  )
  .flatten
  .filter(a => a.freq != '.' && a.freq != '#')

def antiNodes(a: Antenna, b: Antenna) = {
  val xDif = b.x - a.x
  val yDif = b.y - a.y

  var res: List[AntiNode] = List()

  // Move left
  var cx = a.x - xDif
  var cy = a.y - yDif

  while AntiNode(cx, cy).valid
  do
    res = res :+ AntiNode(cx, cy)
    cx = cx - xDif
    cy = cy - yDif

  // Move right
  cx = a.x + xDif
  cy = a.y + yDif

  while AntiNode(cx, cy).valid
  do
    res = res :+ AntiNode(cx, cy)
    cx = cx + xDif
    cy = cy + yDif

  res
}

val res = for
  a1 <- antennas
  a2 <- antennas
  if a1.freq == a2.freq
  if a1 != a2
yield antiNodes(a1, a2).filter(_.valid)

res.flatten.toSet.size

res.flatten.toSet
