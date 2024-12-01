package input

import scala.io.Source

def readFile(name: String): Seq[String] = {
  val f = Source.fromFile(name)
  val lines = f.getLines().toArray
  f.close
  lines
}

def parseTwoInts(lines: Seq[String]): (Seq[Int], Seq[Int]) = {
  val pairs = lines.map(_.split("\\s+"))
  val a = pairs.map(_.apply(0).toInt)
  val b = pairs.map(_.apply(1).toInt)
  (a, b)
}
