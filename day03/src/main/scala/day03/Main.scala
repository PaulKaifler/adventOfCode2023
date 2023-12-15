package day03

import scala.io.Source

def loadPuzzleInput(fromFile: String): List[String] =
  Source.fromResource(fromFile).getLines().toList

def isSymbol(character: Char): Boolean = {
  character match
    case '.'                  => false
    case char if char.isDigit => false
    case _                    => true
}

case class SymbolPosition(x: Int, y: Int)

/**
 * A Number in 2D space
 *
 * @param start Start Index of the Number
 * @param end End Index of the Number
 * @param line The line in which it is
 */
case class NumberPosition(start: Int, end: Int, line: Int)

def findAllSymbols(inputText: List[String]): List[SymbolPosition] = {
  var symbols = List[SymbolPosition]()
  for
    y <- inputText.indices
    x <- 0 until inputText.head.length
    if isSymbol(inputText(y)(x))
  do symbols = symbols :+ SymbolPosition(x, y)
  symbols
}

def findAllNumbers(inputText: List[String]): List[NumberPosition] = {
  var rawNumbers = List[NumberPosition]()

  var startOfNumber: Option[Int] = None
  var endOfNumber: Option[Int] = None

  for (y <- inputText.indices) {
    for (x <- inputText(y).indices) {
      // is it a number
      if inputText(y)(x).isDigit then {
        // is it the start of a new number
        if startOfNumber.isEmpty then
          startOfNumber = Some(x)
          endOfNumber = Some(x)
        else
          // must be the end of a number longer than 1
          endOfNumber = Some(x)
      } else {
        // did we just leave a number
        if startOfNumber.nonEmpty then
          rawNumbers = rawNumbers :+ NumberPosition(start = startOfNumber.get, end = endOfNumber.get, line = y)
          startOfNumber = None
          endOfNumber = None
      }
      if x == inputText(y).length - 1 && startOfNumber.nonEmpty then
        rawNumbers = rawNumbers :+ NumberPosition(start = startOfNumber.get, end = endOfNumber.get, line = y)
        startOfNumber = None
        endOfNumber = None
    }
  }

  rawNumbers
}

@main
def main(): Unit = {
  val gameFile = "day03/input.txt"
  val puzzleText = loadPuzzleInput(gameFile)
  val symbols = findAllSymbols(puzzleText)
  val numbers = findAllNumbers(puzzleText)
  println(numbers)
}
