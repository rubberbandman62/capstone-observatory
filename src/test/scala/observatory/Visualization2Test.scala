package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.Matchers._

trait Visualization2Test extends FunSuite with Checkers {

  test("Visualization2Test#1: simple grid should have two averages 22.2 and -19.2") {
    import Extraction._
    import Manipulation._
    import Visualization2._

    val stationsFile = "testStations2.csv"
    val stations = sc.broadcast(loadStations(stationsFile).collect.toMap)

    import org.apache.log4j.{ Level, Logger }
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

    val yearlyData = for {
      year <- 2023 to 2025
    } yield {
      val temperaturesFile = s"$year.csv"
      val locatedTemperatures = myLocateTemperatures(year, stations, temperaturesFile)
      myLocationYearlyAverageRecords(locatedTemperatures).collect.toSeq
    }

    println()
    println("Now generating grid or tiles for all the years")
    val grid = average(yearlyData)

    val colorScale = Seq(
      (60.0d, Color(255, 255, 255)),
      (7.0d, Color(0, 0, 0)),
      (4.0d, Color(255, 0, 0)),
      (2.0d, Color(255, 255, 0)),
      (0.0d, Color(255, 255, 255)),
      (-2.0d, Color(0, 255, 255)),
      (-7.0d, Color(0, 0, 255)))

    val t0 = System.nanoTime()
    val image = visualizeGrid(grid, colorScale, Tile(0, 0, 0))
    val t1 = System.nanoTime()
    println(s"Creating an image from grid took ${((t1 - t0) / 1e6).toInt / 1000.0d} seconds.") 

    assert(image != null)
    
    val folder = s"target/grids"
    val dir = new java.io.File(folder)
    if (!dir.exists())
      dir.mkdirs()
    val filename = s"$folder/ManipulationTest#2_grid.png"
    val file = new java.io.File(filename)
    image.output(file)
    
    assert(grid(GridLocation(0,0)) === -19.2 +- 0.001)
    assert(grid(GridLocation(45,90)) === 22.2 +- 0.001)
  }

}
