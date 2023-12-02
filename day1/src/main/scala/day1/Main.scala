package day1

import scala.io.Source

val testFile = "day1/smallTest.txt"
val testFile2 = "day1/smallTest2.txt"
val puzzleFile = "day1/input.txt"
def loadPuzzleInput(fromFile: String): List[String] =
  Source.fromResource(fromFile).getLines().toList

def getFirstNumber(string: String): Int =
  val regexPattern = "[0-9]".r
  regexPattern.findFirstIn(string) match
    case Some(n) => n.toInt
    case None    => throw new Exception("No match")

def wordToInt(word: String): Int = {
  word match
    case "one"   => 1
    case "two"   => 2
    case "three" => 3
    case "four"  => 4
    case "five"  => 5
    case "six"   => 6
    case "seven" => 7
    case "eight" => 8
    case "nine"  => 9
}
def getFirstNumberWithWrittenWords(numberString: String, backwards: Boolean = false): Int = {
  val numbers = List("1", "2", "3", "4", "5", "6", "7", "8", "9")
  val validWords = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val reverseNumberString = numberString.reverse

  // Search for real numbers
  var indexOfNumber = Int.MaxValue
  var firstNumber = Int.MaxValue
  if backwards then
    numbers.foreach(number =>
      val index = reverseNumberString.indexOf(number)
      if (index != -1) && (index < indexOfNumber) then
        indexOfNumber = index
        firstNumber = reverseNumberString.charAt(index).asDigit
    )
  else
    numbers.foreach(number =>
      val index = numberString.indexOf(number)
      if (index != -1) && (index < indexOfNumber) then
        indexOfNumber = index
        firstNumber = numberString.charAt(index).asDigit
    )

  // search for written numbers
  var indexOfWrittenNumber = Int.MaxValue
  var firstWrittenNumber = Int.MaxValue
  if backwards then
    val validBackwordWords = validWords.map(_.reverse)
    validBackwordWords.foreach(number =>
      val index = reverseNumberString.indexOf(number)
      if (index != -1) && (index < indexOfWrittenNumber) then
        indexOfWrittenNumber = index
        firstWrittenNumber = wordToInt(number.reverse)
    )
  else
    validWords.foreach(number =>
      val index = numberString.indexOf(number)
      if (index != -1) && (index < indexOfWrittenNumber) then
        indexOfWrittenNumber = index
        firstWrittenNumber = wordToInt(number)
    )

  // compare indexes, choose number
  if indexOfWrittenNumber == -1 then
    println(s"an actual number: ${firstNumber}")
    firstNumber
  else if (indexOfNumber < indexOfWrittenNumber) then firstNumber
  else firstWrittenNumber
}

def tests(): Unit = {
  assert(getFirstNumberWithWrittenWords("two1nine") == 2)
  assert(getFirstNumberWithWrittenWords("two1nine", true) == 9)

  assert(getFirstNumberWithWrittenWords("eightwothree") == 8)
  assert(getFirstNumberWithWrittenWords("eightwothree", true) == 3)

  assert(getFirstNumberWithWrittenWords("abcone2threexyz") == 1)
  assert(getFirstNumberWithWrittenWords("abcone2threexyz", true) == 3)

  assert(getFirstNumberWithWrittenWords("xtwone3four") == 2)
  assert(getFirstNumberWithWrittenWords("xtwone3four", true) == 4)

  assert(getFirstNumberWithWrittenWords("4nineeightseven2") == 4)
  assert(getFirstNumberWithWrittenWords("4nineeightseven2", true) == 2)

  assert(getFirstNumberWithWrittenWords("zoneight234") == 1)
  assert(getFirstNumberWithWrittenWords("zoneight234", true) == 4)

  assert(getFirstNumberWithWrittenWords("7pqrstsixteen") == 7)
  assert(getFirstNumberWithWrittenWords("7pqrstsixteen", true) == 6)
}

@main
def main(): Unit =
  tests()

  val puzzleLines = loadPuzzleInput(puzzleFile)
  var accumulatedCalibration = 0

  puzzleLines.foreach(line =>
    val firstNumber = getFirstNumber(line)
    val lastNumber = getFirstNumber(line.reverse)

    accumulatedCalibration += firstNumber * 10 + lastNumber
  )

  println("Solution Task 1")
  println(accumulatedCalibration)
  println("----")

  accumulatedCalibration = 0
  val puzzleLines2 = loadPuzzleInput(puzzleFile)
  puzzleLines2.foreach(line =>
    val firstNumber = getFirstNumberWithWrittenWords(line)
    val lastNumber = getFirstNumberWithWrittenWords(line, true)

    accumulatedCalibration += (firstNumber * 10 + lastNumber)
  )
  println(("Solution Task 2"))
  println(accumulatedCalibration)
