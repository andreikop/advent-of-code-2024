import aoc.*

val path = "/Users/ak/code/aoc-2024-2/src/day13/input-long.txt"

case class Button(xDif: Int, yDif: Int)
case class Machine(a: Button, b: Button, xGoal: Int, yGoal: Int)

val lines = readFile(path)

def parseMachine(slice: Array[String]) = {
  def parseButton(line: String) = {
    val m = "Button \\w: X\\+(\\d+), Y\\+(\\d+)".r.findFirstMatchIn(line).get
    Button(m.group(1).toInt, m.group(2).toInt)
  }

  val matchPrize = "Prize: X=(\\d+), Y=(\\d+)".r.findFirstMatchIn(slice(2)).get
  Machine(
    parseButton(slice(0)),
    parseButton(slice(1)),
    matchPrize.group(1).toInt,
    matchPrize.group(2).toInt,
  )
}

val machineCount = (lines.length + 1) / 4
val machines = for (machineIndex <- 0 until machineCount)
yield {
  val slice = lines.slice(machineIndex * 4, machineIndex * 4 + 3)
  parseMachine(slice)
}

def solve(m: Machine) = {
  val results = for (
    aCount <- 0 to math.min(100, math.min((m.xGoal / m.a.xDif), (m.yGoal / m.a.yDif)));
    bCount <- 0 to math.min(100, math.min((m.xGoal / m.b.xDif), (m.yGoal / m.b.yDif)));
    xDif <- Seq(m.a.xDif * aCount + m.b.xDif * bCount);
    yDif <- Seq(m.a.yDif * aCount + m.b.yDif * bCount)
    if xDif == m.xGoal && yDif == m.yGoal
  ) yield {
    (aCount, bCount, aCount * 3 + bCount)
  }

  results.sortBy(res => res._3)
  if (results.isEmpty) 0 else results(0)._3
}

val results = machines.map(solve)
results.sum
