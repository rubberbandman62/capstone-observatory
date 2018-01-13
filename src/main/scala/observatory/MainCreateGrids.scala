package observatory

import org.apache.log4j.{ Level, Logger }
import observatory._
import observatory.Extraction._
import observatory.Visualization._
import java.time.LocalDate

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.functions._
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
import scala.collection.JavaConverters._

object MainCreateGrids extends App {

  import Extraction._
  import Manipulation._
  import Interaction._
  import Visualization2._

  def openFile(folder: String, name: String): java.io.File = {
    val dir = new java.io.File(folder)
    if (!dir.exists())
      dir.mkdirs()
    val filename = s"$folder/$name"
    new java.io.File(filename)
  }

  def generateImage(year: Year, tile: Tile, data: GridLocation => Temperature): Unit = {
    val colorScale = Seq(
      (60.0d, Color(255, 255, 255)),
      (7.0d, Color(0, 0, 0)),
      (4.0d, Color(255, 0, 0)),
      (2.0d, Color(255, 255, 0)),
      (0.0d, Color(255, 255, 255)),
      (-2.0d, Color(0, 255, 255)),
      (-7.0d, Color(0, 0, 255)))

    val t0 = System.nanoTime()
    val image = visualizeGrid(data, colorScale, tile)
    val folderYearZoom = s"target/deviations/$year/${tile.zoom}"
    val filename = s"${tile.x}-${tile.y}.png"
    val file = openFile(folderYearZoom, filename)
    image.output(file)
    val t1 = System.nanoTime()
    println(s"image $filename took ${((t1 - t0) * 10000 / 1e9).toInt / 10000.0d} seconds to generate.")
  }

  val stationsFile = "stations.csv"
  val stations = sc.broadcast(loadStations(stationsFile).collect.toMap)

  import org.apache.log4j.{ Level, Logger }
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  println()
  println("calculate yearly averages for the years 1975 to 1990:")

  val t0 = System.nanoTime()
  val normalData = for {
    year <- 1975 to 1990
  } yield {
    val temperaturesFile = s"$year.csv"
    val locatedTemperatures = myLocateTemperatures(year, stations, temperaturesFile)
    myLocationYearlyAverageRecords(locatedTemperatures).collect.toSeq
  }
  val t1 = System.nanoTime()
  println()
  println(s"Calculatig averages for the years 1975 to 1990 took ${((t1 - t0) / 1e6).toInt / 1000.0d} seconds.")

  println()
  println("Now generating grid for normal temperatures calculated from the years 1975 to 1990")
  val t2 = System.nanoTime()
  val normals = average(normalData)
  val t3 = System.nanoTime()
  println()
  println(s"Creating normal temperatures from 1975 to 1990 took ${((t3 - t2) / 1e6).toInt / 1000.0d} seconds.")

  println()
  println("Calculating deviation for the years 1991 to 2015")
  val t4 = System.nanoTime()
  val gridsPerYear = for (year <- 1991 to 2015) yield {
    val t41 = System.nanoTime()
    val temperaturesFile = s"$year.csv"
    val locatedTemperatures = myLocateTemperatures(year, stations, temperaturesFile)
    val averageTemperaturesForYear = myLocationYearlyAverageRecords(locatedTemperatures).collect.toSeq
    val t42 = System.nanoTime()
    println()
    println(s"Creating grids for $year took ${((t42 - t41) / 1e6).toInt / 1000.0d} seconds.")
    (year, deviation(averageTemperaturesForYear, normals))
  }
  val t5 = System.nanoTime()
  println()
  println(s"Creating all the grids for 1991 to 2015 took ${((t5 - t4) / 1e6).toInt / 1000.0d} seconds.")

  val t6 = System.nanoTime()
  generateTiles(gridsPerYear, generateImage)
  val t7 = System.nanoTime()
  println()
  println(s"Creating all the images for 1991 to 2015 took ${((t7 - t6) / 1e6).toInt / 1000.0d} seconds.")

  sc.stop()
}
