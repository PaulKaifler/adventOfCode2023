package day1

import scala.io.Source

val testFile = "day1/smallTest.txt"
val puzzleFile = "day1/input.txt"
def loadPuzzleInput(fromFile: String): List[String] =
  Source.fromResource(fromFile).getLines().toList

def getFirstNumber(of: String): Int =
  val regexPattern = "[0-9]".r
  regexPattern.findFirstIn(of) match
    case Some(n) => n.toInt
    case None => throw new Exception("No match")

@main
def main(): Unit =
  val puzzleLines = loadPuzzleInput(puzzleFile)
  var accumulatedCalibration = 0

  puzzleLines.foreach(line =>
    val backwards = line.reverse
    val firstNumber = getFirstNumber(line)
    val lastNumber = getFirstNumber(backwards)

    accumulatedCalibration += firstNumber * 10 + lastNumber
  )

  println(accumulatedCalibration)
