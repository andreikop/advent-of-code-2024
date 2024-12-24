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

var leftIndex = 0
var rightIndex = diskSize - 1

while leftIndex < rightIndex
do {
  // find space
  while leftIndex < diskSize && disk(leftIndex) != -1
  do leftIndex += 1

  // find busy place
  while rightIndex >= 0 && disk(rightIndex) == -1
  do rightIndex -= 1
  printLine(leftIndex, rightIndex)
  if leftIndex < rightIndex  // if found
  then {
    disk(leftIndex) = disk(rightIndex)
    disk(rightIndex) = -1
  }
  //printDisk
}

printDisk

disk.map(_.toLong).zipWithIndex.map((fileIndex, diskIndex) =>
  if fileIndex == -1 then 0 else fileIndex * diskIndex).sum

// 593815965
// 593815965 first
