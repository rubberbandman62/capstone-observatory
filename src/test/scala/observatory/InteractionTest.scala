package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  def generateImage[A <: Array[(Location, Temperature)]](year: Year, tile: Tile, data: A): Unit = {
    val colorScale = Array(
      (60.0d, Color(255, 255, 255)),
      (32.0d, Color(255, 0, 0)),
      (12.0d, Color(255, 255, 0)),
      (0.0d, Color(0, 255, 255)),
      (-15.0d, Color(0, 0, 255)),
      (-27.0d, Color(255, 0, 255)),
      (-50.0d, Color(33, 0, 107)),
      (-60.0d, Color(0, 0, 0)))
    val t0 = System.nanoTime()
    val image = Interaction.tile(data, colorScale, tile)
    val folderYearZoom = s"target/temperatures/$year/${tile.zoom}"
    val dir = new java.io.File(folderYearZoom)
    if (!dir.exists())
      dir.mkdirs()
    val filename = s"$folderYearZoom/${tile.x}-${tile.y}.png"
    image.output(new java.io.File(filename))
    val t1 = System.nanoTime()
    println(s"tile $filename took ${((t1 - t0) * 10000 / 1e9).toInt / 10000.0d} seconds to generate.")
  }

  test("InteractionTest#1: create the immages for the test year 2021") {

    import Extraction._
    import Interaction._
    import Manipulation._

    val stationsFile = "testStations1.csv"
    val stations = sc.broadcast(loadStations(stationsFile).collect.toMap)

    println()
    println("about to locate temperatures")

    val yearlyData = for {
      year <- 2021 to 2021
    } yield {
      val temperaturesFile = s"$year.csv"
      println("locate temperatures for: " + year)
      val locatedTemperatures = myLocateTemperatures(year, stations, temperaturesFile)
      println("calculate averages for: " + year)
      (year, myLocationYearlyAverageRecords(locatedTemperatures).collect)
    }

    println("Now generating tiles")
    val t0 = System.nanoTime()
    generateTiles(yearlyData, generateImage)
    val t1 = System.nanoTime()
    println(s"It took ${(t1 - t0) / 1e9} seconds to build the tiles")

  }

  test("InteractionTest#2: create the immages year 2022 (very simple)") {
    val colorScale = Array((32.0d, Color(255, 0, 0)),
      (-28.0d, Color(0, 0, 255)))

    import Extraction._
    import Interaction._
    import Manipulation._

    val stationsFile = "testStations2.csv"
    val stations = sc.broadcast(loadStations(stationsFile).collect.toMap)

    println()
    println("about to locate temperatures")

    val yearlyData = for {
      year <- 2022 to 2022
    } yield {
      val temperaturesFile = s"$year.csv"
      println("locate temperatures for: " + year)
      val locatedTemperatures = myLocateTemperatures(year, stations, temperaturesFile)
      locatedTemperatures.collect() foreach (println(_))
      println("calculate averages for: " + year)
      (year, myLocationYearlyAverageRecords(locatedTemperatures).collect)
    }

    println("Now generating tiles")
    val t0 = System.nanoTime()
    generateTiles(yearlyData, generateImage)
    val t1 = System.nanoTime()
    println(s"It took ${(t1 - t0) / 1e9} seconds to build the tiles")

  }

}
