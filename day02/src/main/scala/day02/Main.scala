package day02

import scala.io.Source

class Draw(var r: Int, var g: Int, var b: Int) {
  def changeRed(n: Int): Unit = this.r = n
  def changeGreen(n: Int): Unit = this.g = n
  def changeBlue(n: Int): Unit = this.b = n

  override def toString: String = {
    s"Draw(r: $r, g: $g, b: $b)"
  }
}

case class SetOfCubes(red: Int, green: Int, blue: Int) {
  def power(): Int = {
    red * green * blue
  }

  override def toString: String = s"$red, $green, $blue"
}

case class Game(id: Int, draws: List[Draw]) {
  def validDraws(): Int = {
    draws.foreach(draw => if draw.r > 12 || draw.g > 13 || draw.b > 14 then return 0)
    id
  }

  def getMaxColorOfDraws(): SetOfCubes = {
    var maxRed = Int.MinValue
    var maxGreen = Int.MinValue
    var maxBlue = Int.MinValue

    draws.foreach { draw =>
      if (draw.r > maxRed) maxRed = draw.r
      if (draw.g > maxGreen) maxGreen = draw.g
      if (draw.b > maxBlue) maxBlue = draw.b
    }
    SetOfCubes(maxRed, maxGreen, maxBlue)
  }

  override def toString: String = {
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
  singleDraws.foreach(d =>
    val n = d.split(" ")(1).toInt
    if d contains "red" then draw.changeRed(n)
    else if d contains "green" then draw.changeGreen(n)
    else draw.changeBlue(n)
  )
  draw
}

@main
def main(): Unit = {
  val gameFile = "day02/input.txt"

  val games = createGamesFromFile(gameFile)
  val sum = games.foldLeft(0)((acc, g) => acc + g.validDraws())
  println("Part one")
  println(sum)

  println("\nPart two")
  val setOfCubes = games.map(game => game.getMaxColorOfDraws())
  val powers = setOfCubes.map(set => set.power())
  val sumOfPowers = powers.sum
  println(sumOfPowers)
}
