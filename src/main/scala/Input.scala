package aoc

import scala.io.Source

def readFile(name: String): Array[String] = {
  val f = Source.fromFile(name)
  val lines = f.getLines().toArray
  f.close
  lines
}

def readInts(path: String): Array[Array[Int]] =
  readFile(path).map(_.split("\\s+").map(_.toInt))

def readIntsWoSpaces(path: String): Seq[Array[Int]] =
  readFile(path).map(line => line.toCharArray.map(ch => ch.toString.toInt))

def parseTwoInts(lines: Seq[String]): (Seq[Int], Seq[Int]) = {
  val pairs = lines.map(_.split("\\s+"))
  val a = pairs.map(_.apply(0).toInt)
  val b = pairs.map(_.apply(1).toInt)
  (a, b)
}
