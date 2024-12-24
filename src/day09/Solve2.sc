import aoc.input.*
import aoc.print.*

val path = "/Users/ak/code/aoc-2024/aoc-2024/src/day09/"

val lines = readFile(
  path + "input-long.txt"
)

val nums = lines(0).map(_.toString.toInt).toArray
nums.length

val diskSize = nums.sum
val disk: Array[Int] = Array.fill(diskSize)(-1)

var cursor = 0
for (num, numIndex) <- nums.zipWithIndex
do {
  if numIndex % 2 == 0 // file
  then {
    val fileIndex = numIndex / 2
    val fileLen = num
    for i <- 0 until fileLen
    do disk(cursor + i) = fileIndex
    cursor += fileLen
  } else { // space
    cursor += num
  }
}

def printDisk = println(disk.map(x => if x == -1 then '.' else x).mkString(""))
//printDisk

val Empty = -1

// skip all skipVal and return index of different value or None
def skipLeft(startIndex: Int, skipVal: Int): Int = {
  var index = startIndex
  while index >= 0 && disk(index) == skipVal
  do index -= 1

  index
}

def skipRight(startIndex: Int, skipVal: Int): Int = {
  var index = startIndex
  while index < diskSize && disk(index) == skipVal
  do index += 1

  index
}

def findLeft(startIndex: Int, findVal: Int): Int = {
  var index = startIndex
  while index >= 0 && disk(index) != findVal
  do index -= 1

  index
}

def findRight(startIndex: Int, findVal: Int): Option[Int] = {
  var index = startIndex
  while index < diskSize && disk(index) != findVal
  do index += 1

  if index == diskSize
  then None
  else Some(index)
}

def findGap(size: Int, maxIndex: Int): Option[Int] = {
  var index = 0
  var res: Option[Int] = None
  while index < maxIndex && res.isEmpty
  do {
    findRight(index, Empty) match {
      case Some(spaceIndex) => {
        val firstNonSpaceIndex = skipRight(spaceIndex, Empty)
        val spaceLen = firstNonSpaceIndex - spaceIndex
        if spaceLen >= size && firstNonSpaceIndex <= maxIndex
        then res = Some(spaceIndex)
        index = firstNonSpaceIndex
      }
      case None => index = diskSize
    }
  }
  res
}

def moveFile(from: Int, to: Int, size: Int) = {
  // printLine(from, to, size)
  for i <- 0 until size
  do {
    disk(to + i) = disk(from + i)
    disk(from + i) = Empty
  }
}

var leftIndex = 0
var rightIndex = diskSize - 1


while rightIndex > 0
do {
  // printDisk
  // find busy place
  val fileEnd = skipLeft(rightIndex, Empty)

  if fileEnd > 0 // found a file
  then {
    val fileIndex = disk(fileEnd)
    val fileStart = skipLeft(fileEnd, fileIndex) + 1
    val fileSize = fileEnd - fileStart + 1
  //   printLine("found file", fileIndex, fileStart, fileSize)
    findGap(fileSize, fileStart) match {
      case Some(gapIndex) => moveFile(fileStart, gapIndex, fileSize)
      case None           => {}
    }
    rightIndex = fileStart - 1
  } else {
    rightIndex = -1
  }

}

// printDisk

disk
  .map(_.toLong)
  .zipWithIndex
  .map((fileIndex, diskIndex) =>
    if fileIndex == -1 then 0 else fileIndex * diskIndex
  )
  .sum
