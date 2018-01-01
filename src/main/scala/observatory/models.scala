package observatory

import java.time.LocalDate
import org.apache.commons.math3.util.FastMath._
import scala.collection.parallel.immutable.ParVector

/**
 * Introduced in Week 1. Represents a location on the globe.
 * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
 * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
 */
case class Location(lat: Double, lon: Double) {
//  assert(lon >= -180.0d && lon <= 180.0d, "longitude must be greater or equal -180 and lower or equal +180 (" + lat + ", " + lon + ")")
//  assert(lat <= 90.0d && lat >= -90.0d, "latitude must be greater or equal -90 and lower or equal +90 (" + lat + ", " + lon + ")")

  lazy val phi = lat * PI / 180
  lazy val lambda = lon * PI / 180
  lazy val sinPhi = sin(phi)
  lazy val cosPhi = cos(phi)

  lazy val antipode =
    Location(-this.lat, if (allmostEqual(this.lon, 0.0d)) 180 else signum(this.lon) * (-1) * (180 - abs(this.lon)))

  private def allmostEqual(x: Double, y: Double): Boolean =
    abs(x - y) < epsilon

  // locations with a distance lower than 1000 meters will be handled as equal
  def allmostEqualTo(that: Location): Boolean =
    this.gcDistanceTo(that) == 0.0d

  def isAntipodeOf(that: Location): Boolean =
    this.antipode == that

  def gcDistanceTo(that: Location): Double =
    if (this.isAntipodeOf(that))
      earthRadius * PI
    else {
      val deltaRho = acos(sinPhi * that.sinPhi + cosPhi * that.cosPhi * cos(abs(this.lambda - that.lambda)))
      val distance = earthRadius * deltaRho
      if (distance < 1 /* one kilometer */ ) 0.0d else distance
    }

  def toImageCoordinates(width: Int = 360, height: Int = 180): (Int, Int) =
    ((lon * width / 360 + 180).toInt, (-(this.lat * height / 180) + 90).toInt)

  def toTile(zoom: Short): Tile = new Tile(
    ((lon + 180.0) / 360.0 * (1 << zoom)).toInt,
    ((1 - log(tan(toRadians(lat)) + 1 / cos(toRadians(lat))) / PI) / 2.0 * (1 << zoom)).toInt,
    zoom)

  def this(x: Int, y: Int, width: Int = 360, height: Int = 180) =
    this(-(y - 90) / height.toDouble * 180.0d, (x - 180) / width.toDouble * 360.0d)
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
  def toLocation = new Location(
    toDegrees(atan(sinh(PI * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
    x.toDouble / (1 << zoom) * 360.0 - 180.0)

  def toURI = new java.net.URI("http://tile.openstreetmap.org/" + zoom + "/" + x + "/" + y + ".png")

  private def toListOfRowsOfLocations(level: Int): ParVector[Vector[Location]] = {
    val threshold = 15

    val tileUpperLeft = if (zoom < threshold) Tile(x * 2, y * 2, zoom + 1) else this
    val tileUpperRight = if (zoom < threshold) Tile(x * 2 + 1, y * 2, zoom + 1) else this
    val tileLowerLeft = if (zoom < threshold) Tile(x * 2, y * 2 + 1, zoom + 1) else this
    val tileLowerRight = if (zoom < threshold) Tile(x * 2 + 1, y * 2 + 1, zoom + 1) else this

    if (level <= 1) {
      ParVector(Vector(tileUpperLeft.toLocation, tileUpperRight.toLocation),
        Vector(tileLowerLeft.toLocation, tileLowerRight.toLocation))
    } else {
      val leftList = tileUpperLeft.toListOfRowsOfLocations(level - 1) ++
        tileLowerLeft.toListOfRowsOfLocations(level - 1)

      val rightList = tileUpperRight.toListOfRowsOfLocations(level - 1) ++
        tileLowerRight.toListOfRowsOfLocations(level - 1)

      leftList.zip(rightList).map({ case (l, r) => l ++ r })
    }
  }

  def toListOfLocations(width: Int): ParVector[Location] = {
    val exp = (log10(width) / log10(2.0)).toInt
    this.toListOfRowsOfLocations(exp).foldLeft(ParVector[Location]())((z, l) => z ++ l)
  }

}

/**
 * Introduced in Week 4. Represents a point on a grid composed of
 * circles of latitudes and lines of longitude.
 * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
 * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
 */
case class GridLocation(lat: Int, lon: Int) 

class Grid {
  private val gridData = new Array[Double](Grid.width * Grid.height)

  def get(coordinates: GridLocation): Double =
    gridData(Grid.index(coordinates.lat, coordinates.lon))

  def +(that: Grid): Grid = {
    val newGrid = new Grid
    for (i <- 0 until Grid.width * Grid.height)
      newGrid.gridData(i) = this.gridData(i) + that.gridData(i)
    newGrid
  }
}

object Grid {
  def apply(temperatures: Iterable[(Location, Temperature)]) = {
    val newGrid = new Grid
    for (
      lat <- Grid.miny to Grid.maxy;
      lon <- Grid.minx to Grid.maxx
    ) {
      newGrid.gridData(index(lat, lon)) = Visualization.predictTemperature(temperatures, Location(lat, lon))
    }
    newGrid
  }

  def index(lat: Int, lon: Int) =
    (lat - Grid.miny) * Grid.width + (lon - Grid.minx)

  val width = 360
  val height = 180
  val minx = -180
  val maxx = minx + width - 1
  val miny = -89
  val maxy = miny + height - 1
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
    assert(factor >= 0.0d, "factor must not be negative")
    Color(min(255, (red * factor).toInt),
      min(255, (green * factor).toInt),
      min(255, (blue * factor).toInt))
  }

  def +(that: Color): Color =
    Color(min(255, red + that.red),
      min(255, green + that.green),
      min(255, blue + that.blue))

  def interpolate(that: Color, weight: Double): Color = {
    assert(weight >= 0.0d && weight <= 1.0d)
    val r: Double = round(red * (1 - weight) + that.red * weight)
    val g: Double = round(green * (1 - weight) + that.green * weight)
    val b: Double = round(blue * (1 - weight) + that.blue * weight)
    Color(r.toInt, g.toInt, b.toInt)
  }
  
  def distance(that: Color): Double = {
    sqrt((this.red - that.red) * (this.red - that.red) +
    (this.green - that.green) * (this.green - that.green) +
    (this.blue - that.blue) * (this.blue - that.blue))
  }
}

