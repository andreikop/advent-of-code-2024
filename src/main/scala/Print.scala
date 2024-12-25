package aoc

import scala.collection.immutable.AbstractSeq

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
