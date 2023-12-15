package day02

import scala.io.Source

class Draw(var r: Int, var g: Int, var b: Int) {
  def changeRed(n: Int) = this.r = n
  def changeGreen(n: Int) = this.g = n
  def changeBlue(n: Int) = this.b = n

  override def toString(): String = {
      s"Draw(r: $r, g: $g, b: $b)"
  }
}

case class Game(id: Int, draws: List[Draw]) {
  def validDraws(): Int = {
    draws.foreach(draw =>
      if draw.r > 12 || draw.g > 13 || draw.b > 14 then
        return 0
    )
    id
  }

  override def toString(): String = {
      s"Game(id: $id, draws: $draws)\nIs possible: ${validDraws() == 0}"
  }
}

def loadPuzzleInput(fromFile: String): List[String] =
  Source.fromResource(fromFile).getLines().toList

def createGamesFromFile(file: String): List[Game] = {
  var games = List[Game]()
  val lines = loadPuzzleInput(file)
  lines.foreach(line =>
    val idAndRest = line.split(":")
    val gameId = extractGameId(idAndRest(0))
    val draws = extractDraws(idAndRest(1))
    games = games :+ Game(gameId, draws)
  )
  games
}

def extractGameId(string: String): Int = {
  val id = string.split(" ")
  id(1).toInt
}

def extractDraws(string: String): List[Draw] = {
  var draws = List[Draw]()
  val runs = string.split(";")
  runs.foreach(run => draws = draws :+ stringToDraw(run))
  draws
}

def stringToDraw(string: String): Draw = {
  val singleDraws = string.split(",")
  val draw = new Draw(0, 0, 0)
  singleDraws.foreach( d =>
    val n = d.split(" ")(1).toInt
    if d contains "red" then
      draw.changeRed(n)
    else if d contains "green" then
      draw.changeGreen(n)
    else
      draw.changeBlue(n)
  )
  draw
}

@main
def main() = {
  val gameFile = "day02/input.txt"

  val games = createGamesFromFile(gameFile)
  val sum = games.foldLeft(0)((acc, g) => acc + g.validDraws())
  println(sum)
}