package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Extraction._
import java.time.LocalDate

trait ExtractionTest extends FunSuite {
  
  test("extend the file name") {
    val path1 = fsPath("testStations0.csv")
    val path2 = fsPath("/testStations0.csv")
    assert(path1.endsWith("\\test-classes\\testStations0.csv"))
    assert(path2.endsWith("\\test-classes\\testStations0.csv"))
  }
  
  test("import and clean data") {
    val year = 1999
    val stations = "testStations0.csv"
    val temperatures = "testTemperatures.csv"
    val iter: Iterable[(LocalDate, Location, Temperature)] = locateTemperatures(year, stations, temperatures)
    
    assert(iter.size == 17, "The iterator should include 17 elements")
  }
  
  test("calculate average per year") {
    val year = 1999
    val stations = "testStations0.csv"
    val temperatures = "testTemperatures.csv"
    val records: Iterable[(LocalDate, Location, Temperature)] = locateTemperatures(year, stations, temperatures)
    
    val aver: Iterable[(Location, Temperature)] = locationYearlyAverageRecords(records)
    
    aver.foreach(temp => {
      if (temp._1.lat == 11.1)
        assert((temp._2 * 100).toInt == -583)
      if (temp._1.lat == 22.1)
        assert((temp._2 * 100).toInt == -388)
      if (temp._1.lat == 33.1)
        assert((temp._2 * 100).toInt == -194)
      if (temp._1.lat == 44.1)
        assert((temp._2 * 100).toInt == 0)
      if (temp._1.lat == 0.0)
        assert((temp._2 * 100).toInt == 55)
      if (temp._1.lat == 77.1)
        assert((temp._2 * 100).toInt == 722)
      if (temp._1.lat == 99.1)
        assert((temp._2 * 100).toInt == 166)
    })
    
    assert(aver.size == 7, "There should 7 averages.")
  }
  
//  test("locationYearlyAverageRecords should be able to process 1 million records") {
//    val year = 1975
//    val stations = "stations.csv"
//    val temperatures = "1975.csv"
//    val records  = myLocateTemperatures(year, stations, temperatures)
//    
//    val averages = locationYearlyAverageRecords(records.take(300000))
//    
//    println("Averages: " + averages.size)
//  }
  
}