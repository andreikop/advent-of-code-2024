import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day07/"

val lines = readFile(
  path + "input-long.txt"
)

case class Equation(res: Long, nums: List[Long]) {
  def solvable: Boolean = {
    val opCount = nums.length - 1
    val combinationCount = math.pow(3, opCount).toInt
    val matched: Seq[Boolean] =
      for (combIndex <- 0 until combinationCount)
        yield {
          var acc = nums(0)
          for opIndex <- 0 until opCount
          do {
            val nextNum = nums(opIndex + 1)
            val div = math.pow(3, opIndex).toInt
            val op = (combIndex / div) % 3

            if (op == 0)
              acc *= nextNum
            else if (op == 1)
              acc += nextNum
            else
              acc = (acc.toString + nextNum.toString).toLong
          }
          acc == res
        }
    matched.contains(true)
  }
}

val eqs = lines.map(l => {
  val parts = l.split(":")
  val nums = parts(1).trim.split(" ").map(_.toLong)
  Equation(parts(0).toLong, nums.toList)
})

var globalRes = eqs.filter(_.solvable).map(_.res).sum

eqs(4).solvable
