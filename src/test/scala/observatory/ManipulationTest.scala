package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import observatory.Extraction._
import observatory.Manipulation._

trait ManipulationTest extends FunSuite with Checkers {

//  test("Manipulation: grid should be created for 2015") {
//    val year = 2015
//    val stations = "stations.csv"
//    val temperatures = "2015.csv"
//    val records = myLocateTemperatures(year, stations, temperatures)
//    val averages = myLocationYearlyAverageRecords(records)
//
//    val t0 = System.nanoTime()
//    val gridTemperatur = makeGrid(averages.collect())
//    val t1 = System.nanoTime()
//    val elapsed = ((t1 - t0) * 10000 / 1e9).toInt / 10000.0d
//    
//    println("at 10,10: " + gridTemperatur(GridLocation(10, 10)) + " after " + elapsed + " seconds.")
//  }

}