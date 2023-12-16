package day05

import scala.collection.mutable
import scala.io.Source

def loadPuzzleInput(fromFile: String): List[String] =
  Source.fromResource(fromFile).getLines().toList

case class Mapping(from: BigInt, to: BigInt, length: BigInt) {
  def covers(n: BigInt): Boolean = n >= from && n < from + length

  def translate(n: BigInt): BigInt = to + (n - from)

}

case class MapGroup(startName: String, endName: String, mappings: List[Mapping]) {
  override def toString: String = s"\nfrom: $startName, to: $endName - mappings $mappings"

  def translate(startNumber: BigInt): BigInt = {
    println(s"${startName.toUpperCase()} -> ${endName.toUpperCase()}")
    mappings.foreach { map =>
      if map.covers(startNumber) then {
        println(s"   Translation found:     $startNumber -> ${map.translate(startNumber)}")
        return map.translate(startNumber)
      }
    }
    println(s"   No translation cover:  $startNumber -> $startNumber")
    startNumber
  }
}
case class Almanac(seeds: List[BigInt], mappings: List[MapGroup]) {
  def translate(from: String, to: String, startNumber: BigInt): BigInt = {

    var translation: BigInt = startNumber
    var currentName = from
    while (!currentName.equalsIgnoreCase(to)) {

      val mapGroup = mappings.find(_.startName.equalsIgnoreCase(currentName))
      mapGroup match {
        case Some(group) => {
          translation = group.translate(translation)
          currentName = group.endName
        }
        case None => println(s"This is not a valid start name: $from")
      }
    }
    translation
  }

  def findLowestLocation(from: String): BigInt = {
    val translations = seeds.map(translate(from, "location", _))
    translations.min
  }

  override def toString: String =
    s"""seeds:    $seeds
       |$mappings
       |""".stripMargin
}

object Almanac {
  // Split the input into seeds and map groups
  def apply(almanacString: List[String]): Almanac = {
    // Split the input into seeds and map groups
    val (seedsString :: mapGroupsString) = almanacString.map(_.trim)

    // Parse seeds
    val seeds = seedsString.split("\\s+").toList.tail.map(BigInt(_))

    val regexMapName = """^(.+?)-to-(.+?)\s+map:""".r
    val regexThreeNumbers = """(\d+)\s+(\d+)\s+(\d+)""".r
    var mapGroups = List[MapGroup]()
    var mapData = mapGroupsString.tail

    while (mapData.nonEmpty) {
      var mapFrom = ""
      var mapTo = ""
      mapData.head match {
        case regexMapName(from, to) => mapFrom = from; mapTo = to
        case _                      => throw Exception(s"Couldn't extract maps name $mapData.head")
      }
      mapData = mapData.tail

      var maps = List[Mapping]()
      var i = 0
      while (mapData.indices.contains(i) && !mapData(i).isBlank) {
        mapData(i) match
          case regexThreeNumbers(to, from, interval) =>
            maps = maps :+ Mapping(BigInt(from), BigInt(to), BigInt(interval))
          case _ => throw Exception(s"Trouble parsing the three numbers ${mapData(i)}")

        i += 1
      }
      mapGroups = mapGroups :+ MapGroup(mapFrom, mapTo, maps)

      mapData = mapData.drop(i + 1)
    }

    Almanac(seeds, mapGroups)
  }
}

@main
def main(): Unit = {
  val gameFile = "day05/input.txt"
  val puzzleText = loadPuzzleInput(gameFile)

  val almanac = Almanac(puzzleText)
  println(almanac.findLowestLocation("seed"))
}
