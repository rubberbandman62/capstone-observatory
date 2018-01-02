package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.Matchers._

import scala.math._

import Visualization._

trait VisualizationTest extends FunSuite with Checkers {
  val colorScale = Seq((60.0d, Color(255, 255, 255)),
    (0.0d, Color(0, 255, 255)),
    (12.0d, Color(255, 255, 0)),
    (32.0d, Color(255, 0, 0)),
    (-15.0d, Color(0, 0, 255)),
    (-50.0d, Color(33, 0, 107)),
    (-27.0d, Color(255, 0, 255)),
    (-60.0d, Color(0, 0, 0)))

  val epsilon = 0.000001d

  private def allmostEqual(x: Double, y: Double): Boolean =
    abs(x - y) < epsilon

  test("some close doubles should be allmost equal") {
    val x = 50000d
    val d0 = 5000.0d
    val d1 = d0 + (epsilon + epsilon / x)
    val d2 = d0 - (epsilon + epsilon / x)
    val d3 = d0 + (epsilon - epsilon / x)
    val d4 = d0 - (epsilon - epsilon / x)
    assert(!allmostEqual(d0, d1))
    assert(!allmostEqual(d0, d2))
    assert(allmostEqual(d0, d3))
    assert(allmostEqual(d0, d4))
  }

  test("equal points should have a distance of almost 0") {
    val point1 = Location(90.5 / 2, -250.6 / 4)
    val point2 = Location(45.25, -62.65)
    assert(point1.gcDistanceTo(point2) === 0.0 +- 0.0005)
  }

  test("a point in China is antipode of a point in Argentina") {
    val pointInChina = Location(37, 119)
    val pointInArgentina = Location(-37, -61)
    assert(pointInChina.isAntipodeOf(pointInArgentina))
  }

  test("the antipode of my work place is some where east of New Zealand") {
    val workplace = Location(51.161664, 6.925035)
    val antipode = Location(-51.161664, -173.074965)
    assert(workplace.isAntipodeOf(antipode))
  }

  test("distance between my home and my work place should be about 85.785 kilometers") {
    val point1 = Location(51.7770239, 6.1800854)
    val point2 = Location(51.1617813, 6.9250350)
    assert(point1.gcDistanceTo(point2) === 85.785 +- 0.0005)
  }

  test("the temperature in the center of an equilateral triangle should be the usual average of the temperatures at the corners.") {
    val a = (Location(1.0d, 1.0d), 10.0d)
    val b = (Location(1.0d, 3.0d), 20.0d)
    val c = (Location(2.0d, 3.0d), 30.0d)
    val loc = Location(2.0d, 2.0d)
    val predictedTemp = predictTemperature(Seq(a, b, c), loc)
    assert(predictedTemp === 22.5d +- 0.005d)
  }

  test("the temperature near a given point should be allmost equal to that temperature") {
    val a = (Location(1.0d, 1.0d), 10.0d)
    val b = (Location(1.0d, 3.0d), 20.0d)
    val c = (Location(2.0d, 3.0d), 30.0d)
    val loc = Location(1.006d, 3.006d)
    val predictedTemp = predictTemperature(Seq(a, b, c), loc)
    assert(predictedTemp === 20.0d +- 0.005d)
  }

  test("the color of a temperature corresponding to one of the scale should return that color") {
    val color = interpolateColor(colorScale, 32.0d)
    assert(color == Color(255, 0, 0))
  }

  test("the color of a temperature 22 grad celsius should be (255, 128, 0)") {
    val color = interpolateColor(colorScale, 22.0d)
    assert(color == Color(255, 128, 0))
  }

  test("create some locations with screen coordinates") {
    val la = new Location(x = 0, y = 0)
    val lb = new Location(x = 0, y = 179)
    val lc = new Location(x = 359, y = 179)
    val ld = new Location(x = 359, y = 0)
    val l00 = new Location(x = 180, y = 90)

    assert(la.lon == -180 && la.lat == 90)
    assert(lb.lon == -180 && lb.lat == -89)
    assert(lc.lon == 179 && lc.lat == -89)
    assert(ld.lon == 179 && ld.lat == 90)
    assert(l00.lon == 0.0 && l00.lat == 0.0)
  }

  test("create some locations and convert them to screen coordinates") {
    val locA = Location(90, -180)
    val locB = Location(-89, -180)
    val locC = Location(-89, 179)
    val locD = Location(90, 179)
    val loc00 = Location(0, 0)

    assert(locA.toImageCoordinates().equals((0, 0)))
    assert(locB.toImageCoordinates().equals((0, 179)))
    assert(locC.toImageCoordinates().equals((359, 179)))
    assert(locD.toImageCoordinates().equals((359, 0)))
    assert(loc00.toImageCoordinates().equals((180, 90)))
  }

  test("predict temperature the pixel at -27, -180 (should have 14 Grad)") {
    val top = (Location(54.0d, 0.0d), 32.0d)
    val left = (Location(0.0d, -100.0d), 16.0d)
    val right = (Location(0.0d, -100.0d), 16.0d)
    val bottom = (Location(-54.0d, 0.0d), 0.0d)
    val loc = Location(0.0d, 0.0d)
    val temp = predictTemperature(Seq(top, left, right, bottom), loc)
    println("temperature estimated: " + temp)
    val color = interpolateColor(colorScale, temp)
    assert(color === Color(255, 204, 0))
  }

  test("predict temperature the pixel at -27, -180 with two known temperatures") {
    val red = Color(255, 0, 0)
    val yellow = Color(255, 255, 0)
    val cyan = Color(0, 255, 255)
    val blue = Color(0, 0, 255)
    val simpleColorScale = Seq((50.0d, red), (-15.0d, blue), (12.0d, yellow), (0.0d, cyan))

    val loc1 = Location(-27.0d, -149.62224331d)
    val loc2 = Location(-27.0d, 149.62224331d) 
    
    val loc3 = Location(0.0d, -180.0d)
    val loc4 = Location(-54.0d, -180.0d) 

    val locTemp1 = (loc1, 50.0d)
    val locTemp2 = (loc2, -15.0d)
    val locTemp3 = (loc3, 12.0d)
    val locTemp4 = (loc4, 0.0d)

    val myLocation = Location(-27.0d, -179.0d)
    
    val myLocationToLoc1 = myLocation.gcDistanceTo(loc1)
    val myLocationToLoc2 = myLocation.gcDistanceTo(loc2)
    val myLocationToLoc3 = myLocation.gcDistanceTo(loc3)
    val myLocationToLoc4 = myLocation.gcDistanceTo(loc4)
    
    println(s"from $myLocation to $loc1 it is $myLocationToLoc1")
    println(s"from $myLocation to $loc2 it is $myLocationToLoc2")
    println(s"from $myLocation to $loc3 it is $myLocationToLoc3")
    println(s"from $myLocation to $loc4 it is $myLocationToLoc4")
    
    val temp1 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4), loc1)
    val temp2 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4), loc2)
    val temp3 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4), loc3)
    val temp4 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4), loc4)
    
    val temp = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4), myLocation)

    val color1 = interpolateColor(simpleColorScale, temp1)
    val color2 = interpolateColor(simpleColorScale, temp2)
    val color3 = interpolateColor(simpleColorScale, temp3)
    val color4 = interpolateColor(simpleColorScale, temp4)
    val color  = interpolateColor(simpleColorScale, temp)
    println("color1: " + color1 + " temp1: " + temp1 + " at: " + loc1)
    println("color2: " + color2 + " temp2: " + temp2 + " at: " + loc2)
    println("color3: " + color3 + " temp3: " + temp3 + " at: " + loc3)
    println("color4: " + color4 + " temp4: " + temp4 + " at: " + loc4)
    
    println("color estimated: " + color + " temp: " + temp + " at: " + myLocation)
    println(s"temperature at point1: $temp1")
    println(s"temperature at point2: $temp2")
    println(s"temperature at point3: $temp3")
    println(s"temperature at point4: $temp4")
    println("temperature estimated: " + temp)
   
    assert(color.distance(red) < color.distance(blue), s"$red << $color < $blue")
  }

  test("Grader#2: predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val loc1 = Location(-27.0d, -149.62224331d)
    val loc2 = Location(-27.0d, 149.62224331d) 
    val locTemp1 = (loc1, 50.0d)
    val locTemp2 = (loc2, -15.0d)

    val myLocation = Location(-27.0d, -179.0d)

    val temp = predictTemperature(Seq(locTemp1, locTemp2), myLocation)
    
    assert(abs(temp - 50.0d) < abs(temp + 15.0d), s"estimated temperature $temp should be closer to 50 degrees that to -15 degrees")
  }
  
  test("simulate a grader error with a simplyfied color scale") {
    val red = Color(255, 0, 0)
    val blue = Color(0, 0, 255)
    val simpleColorScale = Seq((50.0d, red), (0.0d, blue))
    
//    val color = interpolateColor(simpleColorScale, 25)
//    println(s"distance of $color (25 degress) to $blue (0 degress): ${color.distance(blue)}")
//    println(s"distance of $color (25 degress) to $red (50 degress): ${color.distance(red)}")
//    assert(color.distance(blue) == color.distance(red))

    val color1 = interpolateColor(simpleColorScale, 25.5)
    println(s"distance of $color1 (25.5 degress) to $blue (0 degrees): ${color1.distance(blue)}")
    println(s"distance of $color1 (25.5 degress) to $red (50 degress): ${color1.distance(red)}")
    assert(color1.distance(blue) > color1.distance(red), "distance to red should lower than distance to blue")
    
//    val color2 = interpolateColor(simpleColorScale, 10.999)
//    println(s"distance of $color2 (10.999 degress) to $blue (0 degress): ${color2.distance(blue)}")
//    println(s"distance of $color2 (10.999 degress) to $red (50 degress): ${color2.distance(red)}")
//    assert(color2.distance(blue) < color2.distance(red), "distance to blue should lower than distance to red")
  }

  test("create an Image from temperatures for year2015") {
    import Extraction._
    val year = 2015
    val stations = "stations.csv"
    val temperatures = "2015.csv"
    val records = myLocateTemperatures(year, stations, temperatures)

    val averages = myLocationYearlyAverageRecords(records)
    // val averagesCollected = averages.collect()

    println("create an image for 2015")
    val t0 = System.nanoTime()
    val image = visualize(averages.collect(), colorScale)
    val t1 = System.nanoTime()
    val elapsed = ((t1 - t0) * 10000 / 1e9).toInt / 10000.0d
    println("image for 2015 generated after " + elapsed + " seconds.")

    assert(image != null)
  }
}
