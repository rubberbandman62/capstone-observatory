package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.Matchers._

import scala.math._

import Visualization._
import com.sksamuel.scrimage.Pixel

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

  test("VisualizationTest#1: some close doubles should be allmost equal") {
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

  test("VisualizationTest#2: equal points should have a distance of almost 0") {
    val point1 = Location(90.5 / 2, -250.6 / 4)
    val point2 = Location(45.25, -62.65)
    assert(point1.gcDistanceTo(point2) === 0.0 +- 0.0005)
  }

  test("VisualizationTest#2: a point in China is antipode of a point in Argentina") {
    val pointInChina = Location(37, 119)
    val pointInArgentina = Location(-37, -61)
    assert(pointInChina.isAntipodeOf(pointInArgentina))
  }

  test("VisualizationTest#4: the antipode of my work place is some where east of New Zealand") {
    val workplace = Location(51.161664, 6.925035)
    val antipode = Location(-51.161664, -173.074965)
    assert(workplace.isAntipodeOf(antipode))
  }

  test("VisualizationTest#5: distance between my home and my work place should be about 85.689 kilometers") {
    val point1 = Location(51.7770239, 6.1800854)
    val point2 = Location(51.1617813, 6.9250350)
    println("distance A: " + point1.altDistanceTo(point2))
    println("distance B: " + point1.gcDistanceTo(point2))
    assert(point1.gcDistanceTo(point2) === 85.689 +- 0.0005)
  }

  test("VisualizationTest#6: the temperature in the center of an equilateral triangle should be the usual average of the temperatures at the corners.") {
    val a = (Location(1.0d, 1.0d), 10.0d)
    val b = (Location(1.0d, 3.0d), 20.0d)
    val c = (Location(2.0d, 3.0d), 30.0d)
    val loc = Location(4.0d / 3.0d, 7.0d / 3.0d)
    val predictedTemp = predictTemperature(Seq(a, b, c), loc)
    assert(predictedTemp === 22.06d +- 0.005d)
  }

  test("VisualizationTest#7: the temperature near a given point should be allmost equal to that temperature") {
    val a = (Location(1.0d, 1.0d), 10.0d)
    val b = (Location(1.0d, 3.0d), 20.0d)
    val c = (Location(2.0d, 3.0d), 30.0d)
    val loc = Location(1.006d, 3.006d)
    val predictedTemp = predictTemperature(Seq(a, b, c), loc)
    assert(predictedTemp === 20.0d +- 0.005d)
  }

  test("VisualizationTest#8: the color of a temperature corresponding to one of the scale should return that color") {
    val color = interpolateColor(colorScale, 32.0d)
    assert(color == Color(255, 0, 0))
  }

  test("VisualizationTest#9: the color of a temperature 22 grad celsius should be (255, 128, 0)") {
    val color = interpolateColor(colorScale, 22.0d)
    assert(color == Color(255, 128, 0))
  }

  test("VisualizationTest#10: Given two colors c1 != c2 and a weight w > 0.5, the interpolated color c1.interpolate(c2, w) should be nearer to c2") {
    val c1 = Color(255, 0, 0)
    val c2 = Color(0, 0, 255)
    val w = 0.6

    val actual = c1.interpolate(c2, w)

    val expRed = (c2.red - c1.red) * w + c1.red
    val expGreen = (c2.green - c1.green) * w + c1.green
    val expBlue = (c2.blue - c1.blue) * w + c1.blue

    val expected = Color(expRed.toInt, expGreen.toInt, expBlue.toInt)

    println("expected: " + expected)
    println("actual: " + actual)
    assert(c1.distance(actual) > c2.distance(actual), s"$actual ($expected) should nearer to $c2 than to $c1")
  }

  test("VisualizationTest#11: create some locations with screen coordinates (360x180)") {
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

  test("VisualizationTest#11a: create some locations with screen coordinates (180x90)") {
    val la = new Location(x = 0, y = 0, 180, 90)
    val lb = new Location(x = 0, y = 89, 180, 90)
    val lc = new Location(x = 179, y = 89, 180, 90)
    val ld = new Location(x = 179, y = 0, 180, 90)
    val l00 = new Location(x = 90, y = 45, 180, 90)

    assert(la.lon == -180 && la.lat == 90, s"$la from (0,0)")
    assert(lb.lon == -180 && lb.lat == -88, s"$lb from (0,89)")
    assert(lc.lon == 178 && lc.lat == -88, s"$lc from (179,89)")
    assert(ld.lon == 178 && ld.lat == 90, s"$ld from (179,0)")
    assert(l00.lon == 0.0 && l00.lat == 0.0, s"$l00 from (45,180)")
  }

  test("VisualizationTest#12: create some locations and convert them to screen coordinates") {
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

  test("VisualizationTest#13: predict temperature the pixel at -27, -180 (should have 14 Grad)") {
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

  test("VisualizationTest#14: predict temperature the pixel at -27, -180 with two known temperatures") {
    val red = Color(255, 0, 0)
    val yellow = Color(255, 255, 0)
    val cyan = Color(0, 255, 255)
    val blue = Color(0, 0, 255)
    val simpleColorScale = Seq((50.0d, red), (-15.0d, blue))

    val loc1 = Location(-27.0d, -149.62224331d) // 50 degrees celsius
    val myLocation1 = Location(-27.0d, -179.9d)
    val loc2 = Location(-27.0d, 149.62224331d) // -15 degrees celsius

    val loc3 = Location(0.0d, -180.0d) // 50 degrees celsius
    val myLocation2 = Location(-26.9d, -180.0d)
    val loc4 = Location(-54.0d, -180.0d) // -15 degrees celsius

    val loc5 = Location(0.0d, -149.62224331d) // 50 degrees celsius
    val myLocation3 = Location(-27.1d, 179.9d)
    val loc6 = Location(-54.0d, 149.62224331d) // -15 degrees celsius

    val locTemp1 = (loc1, 50.0d)
    val locTemp2 = (loc2, -15.0d)
    val locTemp3 = (loc3, 50.0d)
    val locTemp4 = (loc4, -15.0d)
    val locTemp5 = (loc5, 50.0d)
    val locTemp6 = (loc6, -15.0d)

    val myLocationToLoc1 = myLocation1.gcDistanceTo(loc1)
    val myLocationToLoc2 = myLocation1.gcDistanceTo(loc2)
    val myLocationToLoc3 = myLocation1.gcDistanceTo(loc3)
    val myLocationToLoc4 = myLocation1.gcDistanceTo(loc4)
    val myLocationToLoc5 = myLocation1.gcDistanceTo(loc5)
    val myLocationToLoc6 = myLocation1.gcDistanceTo(loc6)

    val myLocationToLoc21 = myLocation2.gcDistanceTo(loc1)
    val myLocationToLoc22 = myLocation2.gcDistanceTo(loc2)
    val myLocationToLoc23 = myLocation2.gcDistanceTo(loc3)
    val myLocationToLoc24 = myLocation2.gcDistanceTo(loc4)
    val myLocationToLoc25 = myLocation2.gcDistanceTo(loc5)
    val myLocationToLoc26 = myLocation2.gcDistanceTo(loc6)

    val myLocationToLoc31 = myLocation3.gcDistanceTo(loc1)
    val myLocationToLoc32 = myLocation3.gcDistanceTo(loc2)
    val myLocationToLoc33 = myLocation3.gcDistanceTo(loc3)
    val myLocationToLoc34 = myLocation3.gcDistanceTo(loc4)
    val myLocationToLoc35 = myLocation3.gcDistanceTo(loc5)
    val myLocationToLoc36 = myLocation3.gcDistanceTo(loc6)

    println(s"from $myLocation1 to $loc1 it is $myLocationToLoc1")
    println(s"from $myLocation1 to $loc2 it is $myLocationToLoc2")
    println(s"from $myLocation1 to $loc3 it is $myLocationToLoc3")
    println(s"from $myLocation1 to $loc4 it is $myLocationToLoc4")
    println
    println(s"from $myLocation2 to $loc1 it is $myLocationToLoc21")
    println(s"from $myLocation2 to $loc2 it is $myLocationToLoc22")
    println(s"from $myLocation2 to $loc3 it is $myLocationToLoc23")
    println(s"from $myLocation2 to $loc4 it is $myLocationToLoc24")
    println
    println(s"from $myLocation3 to $loc5 it is $myLocationToLoc31")
    println(s"from $myLocation3 to $loc6 it is $myLocationToLoc32")

    val temp1 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4, locTemp5, locTemp6), loc1)
    val temp2 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4, locTemp5, locTemp6), loc2)
    val temp3 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4, locTemp5, locTemp6), loc3)
    val temp4 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4, locTemp5, locTemp6), loc4)
    val temp5 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4, locTemp5, locTemp6), loc5)
    val temp6 = predictTemperature(Seq(locTemp1, locTemp2, locTemp3, locTemp4, locTemp5, locTemp6), loc6)

    val myTemp1 = predictTemperature(Seq(locTemp1, locTemp2), myLocation1)
    val myTemp2 = predictTemperature(Seq(locTemp3, locTemp4), myLocation2)
    val myTemp3 = predictTemperature(Seq(locTemp5, locTemp6), myLocation3)

    val color1 = interpolateColor(simpleColorScale, temp1)
    val color2 = interpolateColor(simpleColorScale, temp2)
    val color3 = interpolateColor(simpleColorScale, temp3)
    val color4 = interpolateColor(simpleColorScale, temp4)
    val color5 = interpolateColor(simpleColorScale, temp5)
    val color6 = interpolateColor(simpleColorScale, temp6)

    val myColor1 = interpolateColor(simpleColorScale, myTemp1)
    val myColor2 = interpolateColor(simpleColorScale, myTemp2)
    val myColor3 = interpolateColor(simpleColorScale, myTemp3)

    println("color1: " + color1 + " temp1: " + temp1 + " at: " + loc1)
    println("color2: " + color2 + " temp2: " + temp2 + " at: " + loc2)
    println("color3: " + color3 + " temp3: " + temp3 + " at: " + loc3)
    println("color4: " + color4 + " temp4: " + temp4 + " at: " + loc4)

    println("color estimated: " + myColor1 + " temp: " + myTemp1 + " at: " + myLocation1)
    println("color estimated: " + myColor2 + " temp: " + myTemp2 + " at: " + myLocation2)
    println("color estimated: " + myColor3 + " temp: " + myTemp3 + " at: " + myLocation3)
    println(s"temperature at point1: $temp1")
    println(s"temperature at point2: $temp2")
    println(s"temperature at point3: $temp3")
    println(s"temperature at point4: $temp4")
    println(s"temperature at point4: $temp5")
    println(s"temperature at point4: $temp6")
    println("temperature estimated: " + myTemp1 + " (only two temperatures know at loc1 (50) and loc2 (-15)")
    println("temperature estimated: " + myTemp2 + " (only two temperatures know at loc3 (50) and loc4 (-15)")
    println("temperature estimated: " + myTemp3 + " (only two temperatures know at loc5 (50) and loc6 (-15)")

    assert(myColor1.distance(red) < myColor1.distance(blue), s"$red > $myColor1 >> $blue")
    assert(myColor2.distance(red) < myColor2.distance(blue), s"$red > $myColor2 >> $blue")
    assert(myColor3.distance(red) > myColor3.distance(blue), s"$red >> $myColor3 > $blue")
  }

  test("VisualizationTest#15: predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val loc1 = Location(-27.0d, -149.62224331d)
    val loc2 = Location(-27.0d, 149.62224331d)
    val temp1 = 50.0d
    val temp2 = -15.0d
    val locTemp1 = (loc1, temp1)
    val locTemp2 = (loc2, temp2)

    val myLocation = Location(-27.0d, 179.0d)

    val myTemp = predictTemperature(Seq(locTemp1, locTemp2), myLocation)

    println(s"Distance to location1($loc1) is ${myLocation.gcDistanceTo(loc1)}")
    println(s"Distance to location2($loc2) is ${myLocation.gcDistanceTo(loc2)}")
    println(s"$myTemp should be lower than ${(temp1 + temp2) / 2}")

    assert(abs(myTemp - temp1) > abs(myTemp - temp2), s"estimated temperature $myTemp should be closer to $temp1 degrees that to $temp2 degrees")
  }

  test("VisualizationTest#16: simulate a grader error with a simplyfied color scale") {
    val w = 360
    val h = 180
    val temp0 = 30.636930511720692
    val loc0 = Location(45.0, -90.0)
    val (x0, y0) = loc0.toImageCoordinates(w, h)
    val locTemp0 = (loc0, temp0)
    val red = Color(255, 0, 0)

    val temp1 = 23.214838120089382
    val loc1 = Location(-45.0, 0.0)
    val (x1, y1) = loc1.toImageCoordinates(w, h)
    val locTemp1 = (loc1, temp1)
    val blue = Color(0, 0, 255)

    val simpleColorScale = Seq((temp0, red), (temp1, blue))

    val myLocation = Location(0.0d, -45.0d)
    val myTemp = predictTemperature(Seq(locTemp0, locTemp1), myLocation)
    val color = interpolateColor(simpleColorScale, myTemp)
    println(s"myLocation: distance of myLocation $myLocation to loc0 $loc0 with red color: ${myLocation.gcDistanceTo(loc0)}")
    println(s"myLocation: distance of myLocation $myLocation to loc1 $loc1 with blue color: ${myLocation.gcDistanceTo(loc1)}")
    println(s"myLocation: distance of $color ($myTemp degress) to red $red ($temp1 degress): ${color.distance(red)}")
    println(s"myLocation: distance of $color ($myTemp degress) to blue $blue ($temp0 degrees): ${color.distance(blue)}")
    println()

    val myLoc1 = Location(-26.0,-18.0)
    val myTemp1 = predictTemperature(Seq(locTemp0, locTemp1), myLoc1)
    val color1 = interpolateColor(simpleColorScale, myTemp1)
    println(s"myLoc1: distance of myLoc1 $myLoc1 to loc0 $loc0 with red color: ${myLoc1.gcDistanceTo(loc0)}")
    println(s"myLoc1: distance of myLoc1 $myLoc1 to loc1 $loc1 with blue color: ${myLoc1.gcDistanceTo(loc1)}")
    println(s"myLoc1: distance of $color1 ($myTemp1 degress) to red $red ($temp1 degress): ${color1.distance(red)}")
    println(s"myLoc1: distance of $color1 ($myTemp1 degress) to blue $blue ($temp0 degrees): ${color1.distance(blue)}")

    val image = visualize(Seq(locTemp0, locTemp1), simpleColorScale)
    val filename = s"target/VisualizationTest#16.png"
    image.output(new java.io.File(filename))
    
//    val pixels = image.pixels(x1 -10, y1 -10, 20, 20)
//    for (i <- 0 until 20) {
//      for (j <- 0 until 20) {
//        val p = pixels(i * 20 + j)
//        print(s"(${p.red},${p.blue},${p.green})")
//      }
//      println
//    }
    
    val result1 = image.forall((x: Int, y: Int, p: Pixel) => {
      if (x1 -10 < x && x < x1 +10 && y1 -10 < y && y < y1 +10) {
        val c = Color(p.red, p.green, p.blue)
        if (c.distance(blue) > c.distance(red)) {
          println(s"Error: color $c at ${new Location(x, y, w, h)} ($x,$y) should be closer to blue")
          println(s"examined: near location $loc1 at ($x1,$y1)")
          false
        } else true
      }
      else
        true
    })

    val result0 = image.forall((x: Int, y: Int, p: Pixel) => {
      if (x0 -10 < x && x < x0 +10 && y0 -10 < y && y < y0 +10) {
        val c = Color(p.red, p.green, p.blue)
        if (c.distance(blue) < c.distance(red)) {
          println(s"Error: color $c at ${new Location(x, y, w, h)} ($x,$y) should be closer to red")
          println(s"examined: near location $loc0 at ($x0,$y0)")
          false
        } else true
      }
      else
        true
    })
    assert(result0 && result1)
    assert(color.distance(blue) == color.distance(red), "distance to blue should be equal to the distance to red")
  }

  test("VisualizationTest#17: simulate grader error with screen coordinates") {
    def printSomeResults(loc0: Location, loc1: Location, loc: Location, temp: Temperature, color: Color) = {
      println("location0: " + loc0 + " (10/45)")
      println("location0: " + loc1 + " (350/135)")
      println("test location: " + loc)
      println("distance to loc0: " + loc.gcDistanceTo(loc0))
      println("distance to loc1: " + loc.gcDistanceTo(loc1))
      println("predicted temperature: " + temp + " at location " + loc)
      println("interpolated color: " + color)
    }

    val loc0 = new Location(10, 45, 360, 180)
    val loc1 = new Location(350, 135, 360, 180)
    val temp0 = 1.0
    val temp1 = 44.0

    val locTemp0 = (loc0, temp0)
    val locTemp1 = (loc1, temp1)

    val red = Color(255, 0, 0)
    val blue = Color(0, 0, 255)
    val simpleColorScale = Seq((temp0, red), (temp1, blue))

    //    val test1 = new Location(0.0, -180.0)
    //    val testTemp1 = predictTemperature(Seq(locTemp0, locTemp1), test1)
    //    val testColor1 = interpolateColor(simpleColorScale, testTemp1)
    //    printSomeResults(loc0, loc1, test1, testTemp1, testColor1)
    //    assert(testColor1.distance(blue) == testColor1.distance(red), "distance to blue should equal the distance to red")

    val test2 = new Location(0.0, -179.99)
    val testTemp2 = predictTemperature(Seq(locTemp0, locTemp1), test2)
    val testColor2 = interpolateColor(simpleColorScale, testTemp2)
    printSomeResults(loc0, loc1, test2, testTemp2, testColor2)
    assert(testColor2.distance(blue) > testColor2.distance(red), "distance to blue should greater than the distance to red")

    val test3 = new Location(0.0, 179.99)
    val testTemp3 = predictTemperature(Seq(locTemp0, locTemp1), test3)
    val testColor3 = interpolateColor(simpleColorScale, testTemp3)
    printSomeResults(loc0, loc1, test3, testTemp3, testColor3)
    assert(testColor3.distance(blue) < testColor3.distance(red), "distance to blue should lower than the distance to red")

    val test4 = new Location(0.01, -180.0)
    val testTemp4 = predictTemperature(Seq(locTemp0, locTemp1), test4)
    val testColor4 = interpolateColor(simpleColorScale, testTemp4)
    printSomeResults(loc0, loc1, test4, testTemp4, testColor4)
    assert(testColor4.distance(blue) > testColor4.distance(red), "distance to blue should greater than the distance to red")

    val test5 = new Location(-0.01, -180.0)
    val testTemp5 = predictTemperature(Seq(locTemp0, locTemp1), test5)
    val testColor5 = interpolateColor(simpleColorScale, testTemp5)
    printSomeResults(loc0, loc1, test5, testTemp5, testColor5)
    assert(testColor5.distance(blue) < testColor5.distance(red), "distance to blue should lower than the distance to red")
  }

  test("VisualizationTest#18: create an Image from temperatures for year2021") {
    import Extraction._
    val year = 2021
    val stationsFile = "testStations1.csv"
    val stations = sc.broadcast(loadStations(stationsFile).collect.toMap)

    stations.value.foreach(s => println(s"${s._1._1}/${s._1._1} - ${s._2._1}/${s._2._2}"))

    val temperatures = "2021.csv"
    val records = myLocateTemperatures(year, stations, temperatures)

    val averages = myLocationYearlyAverageRecords(records)
    val averagesCollected = averages.collect()

    println("number of averages: " + averagesCollected.size)
    averagesCollected.foreach(a => println(s"avearge at: ${a._1.lat}/${a._1.lon} - ${a._2}"))

    println("create an image for 2021")
    val t0 = System.nanoTime()
    val image = visualize(averagesCollected, colorScale)
    val t1 = System.nanoTime()
    val elapsed = ((t1 - t0) * 10000 / 1e9).toInt / 10000.0d
    println("image for 2021 generated after " + elapsed + " seconds.")

    val filename = s"target/testImage.png"
    image.output(new java.io.File(filename))

    assert(image != null)
  }

  test("VisualizationTest#19: check all color around one point") {
    val temp0 = -100.0
    val temp1 = -82.2193822909544
    val loc0 = Location(45.0, 45.0)
    val loc1 = Location(-45.0, -45.0)

    val center = Location(0.0, 0.0)

    val red = Color(255, 0, 0)
    val blue = Color(0, 0, 255)
    val locTemp0 = (loc0, temp0)
    val locTemp1 = (loc1, temp1)

    val temperatures = Seq(locTemp0, locTemp1)
    val simpleColorScale = Seq((temp0, red), (temp1, blue))

    for (y <- -90 to 90; x <- -180 to 180) {
      val loc = new Location(y, x)
      val dist0 = loc.gcDistanceTo(loc0)
      val dist1 = loc.gcDistanceTo(loc1)
      val temp = predictTemperature(temperatures, loc)
      val color = interpolateColor(simpleColorScale, temp)
      if (dist0 < dist1) {
        if (color.distance(red) > color.distance(blue)) {
          println(s"Error: $loc => $temp => $color (${dist0 - dist1})")
        }
        assert(color.distance(red) < color.distance(blue), "distance to blue should be greater than the distance to red: " + s"$loc => $temp => $color (${dist0 - dist1})")
      } else if (dist0 > dist1) {
        if (color.distance(red) < color.distance(blue)) {
          println(s"Error: $loc => $temp => $color (${dist0 - dist1})")
        }
        assert(color.distance(red) > color.distance(blue), "distance to blue should be greater than the distance to red: " + s"$loc => $temp => $color (${dist0 - dist1})")
      }

    }

  }

  test("VisualizationTest#20: simple color interpolation") {
    val red = Color(255, 0, 0)
    val blue = Color(0, 0, 255)
    val colorScale = Array((32.0d, red),
      (-28.0d, blue))

    val temp1 = -30
    val temp2 = -25
    val temp3 = +25
    val temp4 = +33

    assert(interpolateColorSimple(colorScale, temp1) == blue)
    assert(interpolateColorSimple(colorScale, temp2) == blue)
    assert(interpolateColorSimple(colorScale, temp3) == red)
    assert(interpolateColorSimple(colorScale, temp4) == red)
  }

  test("VisualizationTest#21: all colors should be red or blue with a simple setting and the simplyfied color interpolation") {
    val red = Color(255, 0, 0)
    val blue = Color(0, 0, 255)

    val temp0 = 32.0
    val temp1 = -28.0
    val loc0 = Location(45.0, 45.0)
    val loc1 = Location(-45.0, -45.0)

    val center = Location(0.0, 0.0)

    val locTemp0 = (loc0, temp0)
    val locTemp1 = (loc1, temp1)

    val temperatures = Seq(locTemp0, locTemp1)
    val simpleColorScale = Seq((temp0, red), (temp1, blue))

    var errorCount = 0
    for (y <- -90 to 90; x <- -180 to 180) {
      val loc = new Location(y, x)
      val dist0 = loc.gcDistanceTo(loc0)
      val dist1 = loc.gcDistanceTo(loc1)
      val temp = predictTemperature(temperatures, loc)
      val color = interpolateColorSimple(simpleColorScale, temp)
      if (dist0 - dist1 < 0) {
        if (color != red) {
          errorCount += 1
          println(s"$loc => $temp => $color (${abs(dist0 - dist1)})")
        }
      } else if (dist0 - dist1 > 0)
        if (color != blue) {
          errorCount += 1
          println(s"$loc => $temp => $color (${abs(dist0 - dist1)})")
        }

    }
    println(s"$errorCount errors found")
  }

}
