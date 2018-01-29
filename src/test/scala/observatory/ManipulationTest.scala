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

  test("ManipulationTest#3: makeGrid must return a grid whose predicted temperatures are consistent with the known temperatures") {
    val loc1 = Location(90.3, -61.4)
    val loc2 = Location(-33.5, -180.5)
    val loc3 = Location(56.3, 179.6)
    val loc4 = Location(-89.7, +15.4)
    val loc5 = Location(17.4, 17.3)
    val loc6 = Location(-37.5, 57.4)
    val loc7 = Location(-69.7, -133.5)
    val loc8 = Location(79.6, -157.7)
    val temp1 = 32.3
    val temp2 = -12.7
    val temp3 = 11.2
    val temp4 = 14.6
    val temp5 = -17.6
    val temp6 = 0.6
    val temp7 = 27.6
    val temp8 = -5.1
    val temperatures = Seq((loc1, temp1), (loc2, temp2), (loc3, temp3), (loc4, temp4), (loc5, temp5), (loc6, temp6), (loc7, temp7), (loc8, temp8))
    val grid = makeGrid(temperatures)
    
    assert(grid(GridLocation(loc1.lat.toInt, loc1.lon.toInt)) === temp1 +- 0.005) 
    assert(grid(GridLocation(loc2.lat.toInt, loc2.lon.toInt)) === temp2 +- 0.005) 
    assert(grid(GridLocation(loc3.lat.toInt, loc3.lon.toInt)) === temp3 +- 0.005) 
    assert(grid(GridLocation(loc4.lat.toInt, loc4.lon.toInt)) === temp4 +- 0.005) 
    assert(grid(GridLocation(loc5.lat.toInt, loc5.lon.toInt)) === temp5 +- 0.005) 
    assert(grid(GridLocation(loc6.lat.toInt, loc6.lon.toInt)) === temp6 +- 0.005) 
    assert(grid(GridLocation(loc7.lat.toInt, loc7.lon.toInt)) === temp7 +- 0.005) 
    assert(grid(GridLocation(loc8.lat.toInt, loc8.lon.toInt)) === temp8 +- 0.005) 
  }

  test("ManipulationTest#4: average must return a grid whose predicted temperatures are the average of the known temperatures") {
    val loc1 = Location(90.3, -61.4)
    val loc2 = Location(-33.5, -180.5)
    val loc3 = Location(56.3, 179.6)
    val loc4 = Location(-89.7, +15.4)
    val loc5 = Location(17.4, 17.3)
    val loc6 = Location(-37.5, 57.4)
    val loc7 = Location(-69.7, -133.5)
    val loc8 = Location(79.6, -157.7)
    
    val temp11 = 32.3
    val temp21 = -12.7
    val temp31 = 11.2
    val temp41 = 14.6
    val temp51 = -14.9
    val temp61 = 24.3
    val temp71 = 4.6
    val temp81 = -4.7
    val temperatures1 = Seq((loc1, temp11), (loc2, temp21), (loc3, temp31), (loc4, temp41), (loc5, temp51), (loc6, temp61), (loc7, temp71), (loc8, temp81))
    
    val temp12 = 52.3
    val temp22 = 42.8
    val temp32 = 31.3
    val temp42 = 14.5
    val temp52 = -24.9
    val temp62 = -24.3
    val temp72 = 14.7
    val temp82 = 8.9
    val temperatures2 = Seq((loc1, temp12), (loc2, temp22), (loc3, temp32), (loc4, temp42), (loc5, temp52), (loc6, temp62), (loc7, temp72), (loc8, temp82))
    
    val temp13 = 37.4
    val temp23 = 12.7
    val temp33 = -15.2
    val temp43 = 24.3
    val temp53 = -4.7
    val temp63 = -13.5
    val temp73 = 11.7
    val temp83 = -11.7
    val temperatures3 = Seq((loc1, temp13), (loc2, temp23), (loc3, temp33), (loc4, temp43), (loc5, temp53), (loc6, temp63), (loc7, temp73), (loc8, temp83))
    
    val average1 = (temp11 + temp12 + temp13) / 3
    val average2 = (temp21 + temp22 + temp23) / 3
    val average3 = (temp31 + temp32 + temp33) / 3
    val average4 = (temp41 + temp42 + temp43) / 3
    val average5 = (temp51 + temp52 + temp53) / 3
    val average6 = (temp61 + temp62 + temp63) / 3
    val average7 = (temp71 + temp72 + temp73) / 3
    val average8 = (temp81 + temp82 + temp83) / 3
    
    val temperaturess = Seq(temperatures1, temperatures2, temperatures3)
    
    val normals = average(temperaturess)
    
    assert(normals(GridLocation(loc1.lat.toInt, loc1.lon.toInt)) === average1 +- 0.005) 
    assert(normals(GridLocation(loc2.lat.toInt, loc2.lon.toInt)) === average2 +- 0.005) 
    assert(normals(GridLocation(loc3.lat.toInt, loc3.lon.toInt)) === average3 +- 0.005) 
    assert(normals(GridLocation(loc4.lat.toInt, loc4.lon.toInt)) === average4 +- 0.005) 
    assert(normals(GridLocation(loc5.lat.toInt, loc5.lon.toInt)) === average5 +- 0.005) 
    assert(normals(GridLocation(loc6.lat.toInt, loc6.lon.toInt)) === average6 +- 0.005) 
    assert(normals(GridLocation(loc7.lat.toInt, loc7.lon.toInt)) === average7 +- 0.005) 
    assert(normals(GridLocation(loc8.lat.toInt, loc8.lon.toInt)) === average8 +- 0.005) 
  }

  test("ManipulationTest#5: deviation must return a grid whose predicted temperatures are the deviations of the known temperatures compared to the normals") {
    val loc1 = Location(90, -61)
    val loc2 = Location(-33, -180)
    val loc3 = Location(56, 179)
    val loc4 = Location(-89, +15)
    val loc5 = Location(17, 17)
    val loc6 = Location(-37, 57)
    val loc7 = Location(-69, -133)
    val loc8 = Location(79, -157)
    
    val temp11 = 32.3
    val temp21 = -12.7
    val temp31 = 11.2
    val temp41 = 14.6
    val temp51 = -14.9
    val temp61 = 24.3
    val temp71 = 4.6
    val temp81 = -4.7
    val temperatures1 = Seq((loc1, temp11), (loc2, temp21), (loc3, temp31), (loc4, temp41), (loc5, temp51), (loc6, temp61), (loc7, temp71), (loc8, temp81))
    
    val temp12 = 52.3
    val temp22 = 42.8
    val temp32 = 31.3
    val temp42 = 14.5
    val temp52 = -24.9
    val temp62 = -24.3
    val temp72 = 14.7
    val temp82 = 8.9
    val temperatures2 = Seq((loc1, temp12), (loc2, temp22), (loc3, temp32), (loc4, temp42), (loc5, temp52), (loc6, temp62), (loc7, temp72), (loc8, temp82))
    
    val temp13 = 37.4
    val temp23 = 12.7
    val temp33 = -15.2
    val temp43 = 24.3
    val temp53 = -4.7
    val temp63 = -13.5
    val temp73 = 11.7
    val temp83 = -11.7
    val temperatures3 = Seq((loc1, temp13), (loc2, temp23), (loc3, temp33), (loc4, temp43), (loc5, temp53), (loc6, temp63), (loc7, temp73), (loc8, temp83))
    
    val temp14 = 37.4
    val temp24 = 12.7
    val temp34 = -15.2
    val temp44 = 24.3
    val temp54 = -4.7
    val temp64 = -13.5
    val temp74 = 11.7
    val temp84 = -11.7
    val loca = Location(39, -157)
    val locb = Location(11, 37)
    val locc = Location(-51, -33)
    val locd = Location(-37, 19)
    val loce = Location(27, -131)
    val locf = Location(17, 177)
    val locg = Location(-57, -31)
    val loch = Location(71, 173)
    val temperaturesA = Seq((loca, temp14), (locb, temp24), (locc, temp34), (locd, temp44), (loce, temp54), (locf, temp64), (locg, temp74), (loch, temp84))
    
    val average1 = (temp11 + temp12 + temp13) / 3
    val average2 = (temp21 + temp22 + temp23) / 3
    val average3 = (temp31 + temp32 + temp33) / 3
    val average4 = (temp41 + temp42 + temp43) / 3
    val average5 = (temp51 + temp52 + temp53) / 3
    val average6 = (temp61 + temp62 + temp63) / 3
    val average7 = (temp71 + temp72 + temp73) / 3
    val average8 = (temp81 + temp82 + temp83) / 3
   
    val temperaturess = Seq(temperatures1, temperatures2, temperatures3)
    
    val normals = average(temperaturess)
    val dev = deviation(temperatures1, normals)
    
    assert(dev(GridLocation(loc1.lat.toInt, loc1.lon.toInt)) === (temp11 - average1) +- 0.001) 
    assert(dev(GridLocation(loc2.lat.toInt, loc2.lon.toInt)) === (temp21 - average2) +- 0.001) 
    assert(dev(GridLocation(loc3.lat.toInt, loc3.lon.toInt)) === (temp31 - average3) +- 0.001) 
    assert(dev(GridLocation(loc4.lat.toInt, loc4.lon.toInt)) === (temp41 - average4) +- 0.001) 
    assert(dev(GridLocation(loc5.lat.toInt, loc5.lon.toInt)) === (temp51 - average5) +- 0.001) 
    assert(dev(GridLocation(loc6.lat.toInt, loc6.lon.toInt)) === (temp61 - average6) +- 0.001) 
    assert(dev(GridLocation(loc7.lat.toInt, loc7.lon.toInt)) === (temp71 - average7) +- 0.001) 
    assert(dev(GridLocation(loc8.lat.toInt, loc8.lon.toInt)) === (temp81 - average8) +- 0.001) 
    
    val devA = deviation(temperaturesA, normals)
    println(s"deviation at $loca is ${devA(GridLocation(loca.lat.toInt, loca.lon.toInt))} while temperature is $temp14 and average is ${normals(GridLocation(loca.lat.toInt, loca.lon.toInt))}")
    println(s"deviation at $locb is ${devA(GridLocation(locb.lat.toInt, locb.lon.toInt))} while temperature is $temp24 and average is ${normals(GridLocation(locb.lat.toInt, locb.lon.toInt))}")
    println(s"deviation at $locc is ${devA(GridLocation(locc.lat.toInt, locc.lon.toInt))} while temperature is $temp34 and average is ${normals(GridLocation(locc.lat.toInt, locc.lon.toInt))}")
    println(s"deviation at $locd is ${devA(GridLocation(locd.lat.toInt, locd.lon.toInt))} while temperature is $temp44 and average is ${normals(GridLocation(locd.lat.toInt, locd.lon.toInt))}")
    println(s"deviation at $loce is ${devA(GridLocation(loce.lat.toInt, loce.lon.toInt))} while temperature is $temp54 and average is ${normals(GridLocation(loce.lat.toInt, loce.lon.toInt))}")
    println(s"deviation at $locf is ${devA(GridLocation(locf.lat.toInt, locf.lon.toInt))} while temperature is $temp64 and average is ${normals(GridLocation(locf.lat.toInt, locf.lon.toInt))}")
    println(s"deviation at $locg is ${devA(GridLocation(locg.lat.toInt, locg.lon.toInt))} while temperature is $temp74 and average is ${normals(GridLocation(locg.lat.toInt, locg.lon.toInt))}")
    println(s"deviation at $loch is ${devA(GridLocation(loch.lat.toInt, loch.lon.toInt))} while temperature is $temp84 and average is ${normals(GridLocation(loch.lat.toInt, loch.lon.toInt))}")
  }
}