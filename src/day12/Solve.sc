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

def perimeterSize(cells: Seq[ChCell]): Int = {
  val ownPos = cells.map(_.pos).toSet
  val nbPos = cells.map(_.allNeighbourPos).flatten.filter(!ownPos(_))
  nbPos.size
}


val prices = for (
  cell <- f.cells
  if ! cell.isVisited
)
yield {
  val plot = collectPlot(cell)
  //printLine(plot, plot.size, perimeterSize(plot))
  plot.length * perimeterSize(plot)
}

prices.sum
