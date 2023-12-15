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

def isGearSymbol(character: Char): Boolean = {
  character == '*'
}

case class SymbolPosition(x: Int, y: Int) {
  // only return the two adjacent symbols if there is exactly two
  def findAdjacentGears(gears: List[GearPosition]): Option[(GearPosition, GearPosition)] = {
    val adjacentGears = gears.filter(gear =>
      (gear.start to gear.end).exists { xPos =>
        Math.abs(gear.line - this.y) <= 1 && Math.abs(xPos - this.x) <= 1
      }
    )
    adjacentGears match {
      case gear1 :: gear2 :: Nil => Some((gear1, gear2))
      case _                     => None
    }
  }
}

/**
 * A Number in 2D space
 *
 * @param start Start Index of the Number
 * @param end End Index of the Number
 * @param line The line in which it is
 */
case class NumberPosition(start: Int, end: Int, line: Int) {
  def isAdjacentToSymbol(symbols: List[SymbolPosition]): Boolean = {
    symbols.exists { symbol =>
      (start to end).exists { xPos =>
        Math.abs(symbol.y - this.line) <= 1 && Math.abs(symbol.x - xPos) <= 1
      }
    }
  }

  def toInt(inString: List[String]): Int = {
    inString(line).substring(start, end + 1).toInt
  }
}

type GearPosition = NumberPosition

def findAllSymbols(inputText: List[String], cond: Char => Boolean): List[SymbolPosition] = {
  var symbols = List[SymbolPosition]()
  for
    y <- inputText.indices
    x <- 0 until inputText.head.length
    if cond(inputText(y)(x))
  do symbols = symbols :+ SymbolPosition(x, y)
  symbols
}

def calcGearRatio(gears: (GearPosition, GearPosition), puzzleText: List[String]): Int = {
  val (gear1, gear2) = gears
  val gear1Ratio = gear1.toInt(puzzleText)
  val gear2Ratio = gear2.toInt(puzzleText)
  gear1Ratio * gear2Ratio
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

/**
 * @param numbers Numbers to check
 * @param symbols Symbol positions
 * @return Return all numbers adjacent to symbols
 */
def findValidNumbers(numbers: List[NumberPosition], symbols: List[SymbolPosition]): List[NumberPosition] =
  numbers.filter(number => number.isAdjacentToSymbol(symbols))

def findGearRatios(gears: List[GearPosition], symbols: List[SymbolPosition], puzzleText: List[String]): List[Int] =
  val foundGears = symbols.map(symbol => symbol.findAdjacentGears(gears)).filter(_.nonEmpty).map(_.get)
  foundGears.map(calcGearRatio(_, puzzleText))
@main
def main(): Unit = {
  val gameFile = "day03/input.txt"
  val puzzleText = loadPuzzleInput(gameFile)
  val symbols = findAllSymbols(puzzleText, isSymbol)
  val numbers = findAllNumbers(puzzleText)
  val validNumbers = findValidNumbers(numbers, symbols).map(_.toInt(puzzleText))
  println("Solution part 1")
  println(validNumbers.sum)

  val gearSymbols = findAllSymbols(puzzleText, isGearSymbol)
  val gears = findGearRatios(numbers, gearSymbols, puzzleText)
  println("\nSolution part 2")
  println(gears.sum)
}
