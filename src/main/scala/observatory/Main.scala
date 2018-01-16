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

object Main extends App {

  val colorScale = Array((60.0d, Color(255, 255, 255)),
    (32.0d, Color(255, 0, 0)),
    (12.0d, Color(255, 255, 0)),
    (0.0d, Color(0, 255, 255)),
    (-15.0d, Color(0, 0, 255)),
    (-27.0d, Color(255, 0, 255)),
    (-50.0d, Color(33, 0, 107)),
    (-60.0d, Color(0, 0, 0)))

  def generateImage[A <: Array[(Location, Temperature)]](year: Year, tile: Tile, data: A): Unit = {
    val t0 = System.nanoTime()
    val image = Interaction.myTile(data, colorScale, tile, scale=1)
    val folderYearZoom = s"target/temperatures/$year/${tile.zoom}"
    val dir = new java.io.File(folderYearZoom)
    if (!dir.exists())
      dir.mkdirs()
    val filename = s"$folderYearZoom/${tile.x}-${tile.y}.png"
    val file = new java.io.File(filename)
    image.output(file)
    val t1 = System.nanoTime()
    println(s"tile $filename took ${((t1 - t0) * 10000 / 1e9).toInt / 10000.0d} seconds to generate.")
  }

  import Extraction._
  import Interaction._
  import Manipulation._

  val stationsFile = "stations.csv"
//  val stations = sc.broadcast(loadStations(stationsFile).collect.toMap)

  import org.apache.log4j.{ Level, Logger }
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  println()
  println("about to locate temperatures")

  val yearlyData = for {
    year <- 2015 to 2015
  } yield {
    val temperaturesFile = s"$year.csv"
    println()
    println("locate temperatures for: " + year)
    val locatedTemperatures = myJoinedLocateTemperatures(year, stationsFile, temperaturesFile)
    println()
    println("calculate averages for: " + year)
    (year, myLocationYearlyAverageRecords(locatedTemperatures).collect)
  }

  println()
  println("Now generating grid or tiles for all the years")
  val t0 = System.nanoTime()
  generateTiles(yearlyData, generateImage)
  //  val getTemp = makeGrid(averageTemperaturesPerStation2015.collect)
  val t1 = System.nanoTime()
  println(s"It took ${(t1 - t0) / 1e9} seconds to build the grid/tiles for 2015")
  //  println(s"Temperature at (10,10): ${getTemp(GridLocation(10, 10))}")
  //  println(s"Temperature at (0,0): ${getTemp(GridLocation(0, 0))}")
  //  println(s"Temperature at (-10,-10): ${getTemp(GridLocation(-10, -10))}")
  //  println(s"Temperature at (10,-10): ${getTemp(GridLocation(10, -10))}")
  //  println(s"Temperature at (-10,10): ${getTemp(GridLocation(-10, 10))}")

  sc.stop()
}
