package day04

import scala.collection.mutable
import scala.io.Source

def loadPuzzleInput(fromFile: String): List[String] =
  Source.fromResource(fromFile).getLines().toList

case class Card(cardNumber: Int, winningNumbers: List[Int], numbersIHave: List[Int]) {
  override def toString: String = s"Card no. $cardNumber, $winningNumbers, drawn numbers: $numbersIHave"

  def calcPoints(): Int = {
    val hits = getHits()
    hitsToPoints(hits)
  }

  def getHits(): Int = {
    var hits = 0
    numbersIHave.foreach { number =>
      if winningNumbers.contains(number) then hits += 1
    }
    hits
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

def winCards(cards: List[Card]): mutable.HashMap[Int, Int] = {
  val amountMap = new mutable.HashMap[Int, Int]()
  cards.foreach { card =>
    val hits = card.getHits()
    val cardNumber = card.cardNumber
    if (!amountMap.contains(cardNumber)) amountMap.update(cardNumber, 1)
    val numberOfCopies = amountMap.getOrElse(cardNumber, 1)

    (0 until hits).foreach { index =>
      val cardIncreasing = cardNumber + index + 1
      if (!amountMap.contains(cardIncreasing)) {
        amountMap.update(cardIncreasing, 1)
      }
      amountMap(cardIncreasing) += numberOfCopies
    }
  }
  amountMap
}

@main
def main(): Unit = {
  val gameFile = "day04/input.txt"
  val puzzleText = loadPuzzleInput(gameFile)

  val cards = puzzleText.map(Card(_))
  val pointsPerCard = cards.map(_.calcPoints())
  println("Solution part 1")
  println(pointsPerCard.sum)

  println("---")
  val map = winCards(cards)
  println("Solution part 2")
  println(map.values.sum)
}
