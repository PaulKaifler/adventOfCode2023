package day04

import scala.io.Source

def loadPuzzleInput(fromFile: String): List[String] =
  Source.fromResource(fromFile).getLines().toList

case class Card(cardNumber: Int, winningNumbers: List[Int], numbersIHave: List[Int]) {
  override def toString: String = s"Card no. $cardNumber, $winningNumbers, drawn numbers: $numbersIHave"

  def calcPoints(): Int = {
    var hits = 0
    numbersIHave.foreach { number =>
      if winningNumbers.contains(number) then hits += 1
    }
    hitsToPoints(hits)
  }

  private def hitsToPoints(hits: Int): Int = if (hits < 2) hits else Math.pow(2, hits - 1).toInt
}

object Card {
  def apply(cardString: String): Card = {
    val parts = cardString.split(":")
    val cardNumber = parts(0).split("\\s+")(1).toInt
    val numbers = parts(1).split("\\|")
    val winningNumbers = numbers(0).trim.split("\\s+").map(_.toInt).toList
    val numbersIHave = numbers(1).trim.split("\\s+").map(_.toInt).toList
    Card(cardNumber, winningNumbers, numbersIHave)
  }
}

@main
def main(): Unit = {
  val gameFile = "day04/input.txt"
  val puzzleText = loadPuzzleInput(gameFile)

  val cards = puzzleText.map(Card(_))
  val pointsPerCard = cards.map(_.calcPoints())
  println(pointsPerCard.sum)
}
