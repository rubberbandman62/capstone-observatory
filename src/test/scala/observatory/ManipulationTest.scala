package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.Matchers._
import observatory.Extraction._
import observatory.Manipulation._

trait ManipulationTest extends FunSuite with Checkers {

  test("ManipulationTest#1: grid should be created for 2015") {
    val year = 2015
    val stationsFile = "stations.csv"
    val temperaturesFile = "2015.csv"

//    val stations = sc.broadcast(loadStations(stationsFile).collect.toMap)

    val records = myJoinedLocateTemperatures(year, stationsFile, temperaturesFile)
    val averages = myLocationYearlyAverageRecords(records)

    val t0 = System.nanoTime()
    val gridTemperatur = makeGrid(averages.collect())
    val t1 = System.nanoTime()
    val elapsed = ((t1 - t0) * 10000 / 1e9).toInt / 10000.0d

    println("at 10,10: " + gridTemperatur(GridLocation(10, 10)) + " after " + elapsed + " seconds.")
  }

  test("ManipulationTest#2: simple grid should have two averages 22.2 and -19.2") {
    import Extraction._
    import Interaction._
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
    val t0 = System.nanoTime()
    val grid = average(yearlyData)
    val t1 = System.nanoTime()
    println(s"It took ${(t1 - t0) / 1e9} seconds to build the average for the years 2023 till 2025")
    
    println(s"distance from (0/0) to (21/42): ${Location(0, 0).gcDistanceTo(Location(21, 42))}")
    println(s"distance from (45/90) to (21/42): ${Location(45, 90).gcDistanceTo(Location(21, 42))}")
    println(s"Temperature at (21/42): ${grid(GridLocation(21,42))}")

    assert(grid(GridLocation(0,0)) === -19.2 +- 0.001)
    assert(grid(GridLocation(45,90)) === 22.2 +- 0.001)
  }

}