import aoc.*

val path = "/Users/ak/code/aoc-2024-2/src/day12/input-long.txt"

val f = readCharField(path)

def collectPlot(c: ChCell): Seq[ChCell] = {
  c.markVisited
  val directNbs = for (
    nc <- c.validNeighbours;
    if nc.value == c.value
    if ! nc.isVisited
  ) yield {
    nc.markVisited
    nc
  }

  Seq(c) ++ directNbs.map(nb => collectPlot(nb)).flatten
}

case class Section(dir: Char, pos: Pos) {
  def next = {
    val nextPos = if (dir == 'l' || dir == 'r') then pos.down else pos.right
    Section(dir, nextPos)
  }
}

def direction(a: Pos, b: Pos): Char = {
  val xDif = a.x - b.x
  val yDif = a.y - b.y
  (xDif, yDif) match {
    case (-1, 0) => 'l'
    case (1, 0) => 'r'
    case (0, 1) => 'u'
    case (0, -1) => 'd'
  }
}

def perimeter(cells: Seq[ChCell]): Seq[Section] = {
  val ownPos = cells.map(_.pos).toSet
  def makeSection(cell: ChCell, nbPos: Pos) = Section(
    direction(cell.pos, nbPos), nbPos)
  val sections = cells.map(
    cell => cell.allNeighbourPos.map(
      nbPos => makeSection(cell, nbPos)))
    .flatten
    .filter(section => !ownPos(section.pos))
  sections
}

def applyDiscount(sections: Seq[Section]): Set[Section] = {
  sections.toSet -- sections.map(_.next).toSet
}

val prices = for (
  cell <- f.cells
  if ! cell.isVisited
)
yield {
  val plot = collectPlot(cell)
  val sections = perimeter(plot)
  val withDiscount = applyDiscount(sections)
  //printLine(plot, plot.size, perimeterSize(plot))
  plot.length * withDiscount.size
}

prices.sum
