package observatory

import org.apache.commons.math3.util.FastMath.PI
import org.apache.commons.math3.util.FastMath.abs
import org.apache.commons.math3.util.FastMath.acos
import org.apache.commons.math3.util.FastMath.atan
import org.apache.commons.math3.util.FastMath.atan2
import org.apache.commons.math3.util.FastMath.cos
import org.apache.commons.math3.util.FastMath.log
import org.apache.commons.math3.util.FastMath.log10
import org.apache.commons.math3.util.FastMath.min
import org.apache.commons.math3.util.FastMath.round
import org.apache.commons.math3.util.FastMath.signum
import org.apache.commons.math3.util.FastMath.sin
import org.apache.commons.math3.util.FastMath.sinh
import org.apache.commons.math3.util.FastMath.sqrt
import org.apache.commons.math3.util.FastMath.tan
import org.apache.commons.math3.util.FastMath.toDegrees
import org.apache.commons.math3.util.FastMath.toRadians
import scala.collection.mutable.HashMap

/**
 * Introduced in Week 1. Represents a location on the globe.
 * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
 * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
 */
case class Location(lat: Double, lon: Double) {
  //  assert(lon >= -180.0d && lon <= 180.0d, "longitude must be greater or equal -180 and lower or equal +180 (" + lat + ", " + lon + ")")
  //  assert(lat <= 90.0d && lat >= -90.0d, "latitude must be greater or equal -90 and lower or equal +90 (" + lat + ", " + lon + ")")

  //  lazy val phi = lat * PI / 180
  lazy val phi = toRadians(lat)
  //  lazy val lambda = lon * PI / 180
  lazy val lambda = toRadians(lon)
  lazy val sinPhi = sin(phi)
  lazy val cosPhi = cos(phi)

  lazy val antipode =
    Location(-this.lat, if (allmostEqual(this.lon, 0.0d)) 180 else signum(this.lon) * (-1) * (180 - abs(this.lon)))

  private def allmostEqual(x: Double, y: Double): Boolean =
    abs(x - y) < epsilon

  def isAntipodeOf(that: Location): Boolean =
    this.antipode == that

  def altDistanceTo(that: Location): Double = {
    val dLat = (that.lat - lat).toRadians
    val dLon = (that.lon - lon).toRadians

    val a = sin(dLat / 2) * sin(dLat / 2) + cos(lat.toRadians) * cos(that.lat.toRadians) * sin(dLon / 2) * sin(dLon / 2)
    val c = 2 * atan2(sqrt(a), sqrt(1 - a))

    earthRadius * c
  }

  def gcDistanceTo(that: Location): Double =
    if (this.isAntipodeOf(that))
      earthRadius * PI
    else {
      val deltaRho = acos(sinPhi * that.sinPhi + cosPhi * that.cosPhi * cos(abs(this.lambda - that.lambda)))
      earthRadius * deltaRho
    }

  def toImageCoordinates(width: Int = 360, height: Int = 180): (Int, Int) =
    (((lon + 180) * width / 360).toInt, ((90 - lat) * height / 180).toInt)

  def toTile(zoom: Short): Tile = new Tile(
    ((lon + 180.0) / 360.0 * (1 << zoom)).toInt,
    ((1 - log(tan(toRadians(lat)) + 1 / cos(toRadians(lat))) / PI) / 2.0 * (1 << zoom)).toInt,
    zoom)

  def this(x: Int, y: Int, width: Int = 360, height: Int = 180) = {
    this(90 - (y / height.toDouble * 180.0d), (x / width.toDouble * 360.0d) - 180)
  }

  //  def this(x: Int, y: Int, width: Int = 360, height: Int = 180) = {
  //    this((90 - y) / height.toDouble * 180.0d, (x - 180) / width.toDouble * 360.0d)
  //  }
}

/**
 * Introduced in Week 3. Represents a tiled web map tile.
 * See https://en.wikipedia.org/wiki/Tiled_web_map
 * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
 * @param x X coordinate of the tile
 * @param y Y coordinate of the tile
 * @param zoom Zoom level, 0 ≤ zoom ≤ 19
 */
case class Tile(x: Int, y: Int, zoom: Int) {
  lazy val toLocation = new Location(
    toDegrees(atan(sinh(PI * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
    x.toDouble / (1 << zoom) * 360.0 - 180.0)

  def toURI = new java.net.URI("http://tile.openstreetmap.org/" + zoom + "/" + x + "/" + y + ".png")

  private def toListOfRowsOfLocations(level: Int): Seq[Seq[Location]] = {
    if (zoom + level >= 19)
      Seq(Seq(this.toLocation, this.toLocation), Seq(this.toLocation, this.toLocation))
    else {
      val tileUpperLeft = Tile(x * 2, y * 2, zoom + 1)
      val tileUpperRight = Tile(x * 2 + 1, y * 2, zoom + 1)
      val tileLowerLeft = Tile(x * 2, y * 2 + 1, zoom + 1)
      val tileLowerRight = Tile(x * 2 + 1, y * 2 + 1, zoom + 1)

      if (level <= 1) {
        Seq(Seq(tileUpperLeft.toLocation, tileUpperRight.toLocation),
            Seq(tileLowerLeft.toLocation, tileLowerRight.toLocation))
      } else {
        val leftList = tileUpperLeft.toListOfRowsOfLocations(level - 1) ++
                       tileLowerLeft.toListOfRowsOfLocations(level - 1)

        val rightList = tileUpperRight.toListOfRowsOfLocations(level - 1) ++
                        tileLowerRight.toListOfRowsOfLocations(level - 1)

        leftList.zip(rightList).map({ case (l, r) => l ++ r })
      }
    }
  }

  def toListOfLocations(width: Int): Seq[Location] = {
    val exp = (log10(width) / log10(2.0)).toInt
    this.toListOfRowsOfLocations(exp).foldLeft(Seq[Location]())((z, l) => z ++ l)
  }

}

/**
 * Introduced in Week 4. Represents a point on a grid composed of
 * circles of latitudes and lines of longitude.
 * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
 * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
 */
case class GridLocation(lat: Int, lon: Int) {
  assert( -90 < lat && lat <=  90 && 
         -180 <= lon && lon < 180, s"latitude $lat or longitude $lon not in the expected range.")
}

//case class ArrayGrid() {
//  private val dataSize = (ArrayGrid.width / ArrayGrid.resolution) * (ArrayGrid.height / ArrayGrid.resolution)
//
//  private val data = new Array[Temperature](dataSize)
//
//  def get(gl: GridLocation): Temperature = 
//    data(ArrayGrid.index(gl.lat, gl.lon))
//    
//  def +(that: ArrayGrid): ArrayGrid = {
//    val newGrid = ArrayGrid()
//    for (i <- 0 until dataSize) 
//      newGrid.data(i) = this.data(i) + that.data(i)
//    newGrid
//  }
//
//  def /(number: Double): ArrayGrid = {
//    val newGrid = ArrayGrid()
//    for (i <- 0 until dataSize) 
//      newGrid.data(i) = this.data(i) / number
//    newGrid
//  }
//
//}
//
//object ArrayGrid {
//  val resolution: Int = 2
//  val width: Int = 360
//  val height: Int = 180
//  
//  private val minLon = -width / 2
//  private val maxLon = minLon + width - 1
//  private val minLat = -height / 2 + 1
//  private val maxLat = minLat + height - 1
//  
//  def index(lat: Int, lon: Int) =
//    -(lat / resolution - maxLat / resolution) * width / resolution + lon / resolution - minLon / resolution
//    
//  def apply(temperatures: Iterable[(Location, Temperature)]) = {
//    val newGrid = new ArrayGrid()
//    for (
//      lat <- maxLat to minLat by -resolution;
//      lon <- minLon to maxLon by resolution
//    ) newGrid.data(index(lat, lon)) = Visualization.predictTemperature(temperatures, Location(lat, lon))
//    newGrid
//  }
//
//  def findGridTemperatures(g: GridLocation => Temperature, loc: Location): (Temperature, Temperature, Temperature, Temperature) = {
//    val top = if (loc.lat < maxLat) (loc.lat + 1).toInt else maxLat
//    val left = if (loc.lon > minLon) (loc.lon - 1).toInt else minLon
//    val bottom = top - 1
//    val right = left + 1
//    val d00 = g(GridLocation(top, left))
//    val d10 = g(GridLocation(top, right))
//    val d01 = g(GridLocation(bottom, left))
//    val d11 = g(GridLocation(bottom, right))
//    (d00, d10, d01, d11)
//  }
//  
//}
//
//class MapGrid(val data: Map[GridLocation, Temperature]) {
//
//  def get(gridLocation: GridLocation): Double = {
//    val dlat = abs(gridLocation.lat % MapGrid.resolution)
//    val dlon = abs(gridLocation.lon % MapGrid.resolution)
//    val lat = gridLocation.lat + dlat
//    val lon = gridLocation.lon - dlon
//    data(GridLocation(lat, lon))
//  }
//
//  def +(that: MapGrid): MapGrid =
//    new MapGrid((for (location <- data.keys) yield (location, data(location) + that.data(location))).toMap)
//
//  def /(x: Int): MapGrid =
//    new MapGrid((for (location <- data.keys) yield (location, data(location) / x)).toMap)
//}
//
//object MapGrid {
//  val resolution = 1
//
//  def apply(temperatures: Iterable[(Location, Temperature)]) = {
//    val newGrid = for {
//      lat <- MapGrid.maxy to MapGrid.miny by -resolution
//      lon <- MapGrid.minx to MapGrid.maxx by resolution
//    } yield {
//      val gridLocation = GridLocation(lat, lon)
//      val temp = Visualization.predictTemperature(temperatures, Location(lat, lon))
//      (gridLocation, temp)
//    }
//    new MapGrid(newGrid.toMap)
//  }
//  
//  def empty() = {
//    val newGrid = for {
//      lat <- MapGrid.maxy to MapGrid.miny by -resolution
//      lon <- MapGrid.minx to MapGrid.maxx by resolution
//    } yield {
//      val gridLocation = GridLocation(lat, lon)
//      (gridLocation, 0.0d)
//    }
//    new MapGrid(newGrid.toMap)
//  }
//
//  def findGridTemperatures(g: GridLocation => Temperature, loc: Location): (Temperature, Temperature, Temperature, Temperature) = {
//    val top = if (loc.lat < maxy) (loc.lat + 1).toInt else maxy
//    val left = if (loc.lon > minx) (loc.lon - 1).toInt else minx
//    val bottom = top - 1
//    val right = left + 1
//    val d00 = g(GridLocation(top, left))
//    val d10 = g(GridLocation(top, right))
//    val d01 = g(GridLocation(bottom, left))
//    val d11 = g(GridLocation(bottom, right))
//    (d00, d10, d01, d11)
//  }
//
//  val width = 360
//  val height = 180
//  val minx = -width / 2
//  val maxx = minx + width - 1
//  val miny = -height / 2 + 1
//  val maxy = miny + height - 1
//  val dataSize = (width / resolution) * (height / resolution)
//}

case class CachedGrid(val temperatures: Iterable[(Location, Temperature)]) {
  
  private val data = HashMap[GridLocation, Temperature]()
  val iterator = data.iterator

  def get(gridLocation: GridLocation): Double = {
    val dlat = abs(gridLocation.lat % CachedGrid.resolution)
    val dlon = abs(gridLocation.lon % CachedGrid.resolution)
    val lat = gridLocation.lat + dlat
    val lon = gridLocation.lon - dlon
    val key = GridLocation(lat, lon)
    data.getOrElse(key, {
      val temp = Visualization.predictTemperature(temperatures, Location(lat, lon))
      data.put(key, temp)
      temp
    })
  }
  
  def this(restoredData: Map[GridLocation, Temperature]) = {
    this(restoredData.toIterable.map({ case (gl, temp) => (Location(gl.lat, gl.lon), temp) }))
    restoredData.foreach({ case (gl, temp) => this.data.put(gl, temp) })
  }
}

object CachedGrid {
  val resolution = 2

  def findGridTemperatures(g: GridLocation => Temperature, loc: Location): ((GridLocation,Temperature), (GridLocation,Temperature), (GridLocation,Temperature), (GridLocation,Temperature)) = {
    val top = scala.math.ceil(loc.lat).toInt
    val left = scala.math.floor(loc.lon).toInt
    val bottom = top - 1
    val right = if (left < maxx) left + 1 else minx 
    val glTopLeft = GridLocation(top, left)
    val glTopRight = GridLocation(top, right)
    val glBottomRight = GridLocation(bottom, right)
    val glBottomLeft = GridLocation(bottom, left) 
    ((glBottomLeft,g(glBottomLeft)), 
     (glBottomRight,g(glBottomRight)), 
     (glTopLeft,g(glTopLeft)), 
     (glTopRight,g(glTopRight)))
  }

  lazy val gridLocations = {
    for {
      lat <- maxy to miny by -resolution
      lon <- minx to maxx by resolution
    } yield GridLocation(lat, lon)
  }
  
  val width = 360
  val height = 180
  val minx = -width / 2
  val maxx = minx + width - 1
  val miny = -height / 2 + 1
  val maxy = miny + height - 1
  val dataSize = (width / resolution) * (height / resolution)
}

/**
 * Introduced in Week 5. Represents a point inside of a grid cell.
 * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
 * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
 */
case class CellPoint(x: Double, y: Double)

/**
 * Introduced in Week 2. Represents an RGB color.
 * @param red Level of red, 0 ≤ red ≤ 255
 * @param green Level of green, 0 ≤ green ≤ 255
 * @param blue Level of blue, 0 ≤ blue ≤ 255
 */
case class Color(red: Int, green: Int, blue: Int) {
  def *(factor: Double): Color = {
    // assert(factor >= 0.0d, "factor must not be negative")
    Color(min(255, (red * factor).toInt),
      min(255, (green * factor).toInt),
      min(255, (blue * factor).toInt))
  }

  def +(that: Color): Color =
    Color(min(255, red + that.red),
      min(255, green + that.green),
      min(255, blue + that.blue))

  def interpolate(that: Color, weight: Double): Color = {
    // assert(weight >= 0.0d && weight <= 1.0d)
    val r: Double = round(red + weight * (that.red - red))
    val g: Double = round(green + weight * (that.green - green))
    val b: Double = round(blue + weight * (that.blue - blue))
    Color(r.toInt, g.toInt, b.toInt)
  }

  def distance(that: Color): Double = {
    sqrt((this.red - that.red) * (this.red - that.red) +
      (this.green - that.green) * (this.green - that.green) +
      (this.blue - that.blue) * (this.blue - that.blue))
  }

}

