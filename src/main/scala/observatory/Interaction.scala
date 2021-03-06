package observatory

import com.sksamuel.scrimage.{ Image, Pixel }
import com.sksamuel.scrimage.ScaleMethod
import org.apache.spark.rdd.RDD
import org.apache.commons.math3.util.FastMath._

/**
 * 3rd milestone: interactive visualization
 */
object Interaction {

  /**
   * @param tile Tile coordinates
   * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
   */
  def tileLocation(tile: Tile): Location =
    tile.toLocation

  /**
   * @param temperatures Known temperatures
   * @param colors Color scale
   * @param tile Tile coordinates
   * @return A 256×256 image showing the contents of the given tile
   */
  def tile(temperatures: Iterable[(Location, Temperature)],
           colors: Iterable[(Temperature, Color)],
           tile: Tile): Image =
    myTile(temperatures, colors, tile)

  def myTile(temperatures: Iterable[(Location, Temperature)],
             colors: Iterable[(Temperature, Color)],
             tile: Tile,
             scale: Int = 1,
             transparency: Int = 127): Image = {
    import Visualization._
    val width = 256 / scale
    val height = 256 / scale
    val pixels = tile.toListOfLocations(width).par.map(location => {
      val temp = predictTemperature(temperatures, location)
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, transparency)
    })

    Image(width, height, pixels.toArray).scale(scale, ScaleMethod.FastScale)
  }

  def tileSimple(temperatures: Iterable[(Location, Temperature)],
                 colors: Iterable[(Temperature, Color)],
                 tile: Tile): Image = {
    import Visualization._
    val width = 256
    val transparency = 127
    val pixels = tile.toListOfLocations(width).map(location => {
      val temp = predictTemperature(temperatures, location)
      val color = interpolateColorSimple(colors, temp)
      Pixel(color.red, color.green, color.blue, transparency)
    })

    Image(width, width, pixels.toArray) //.scale(2, ScaleMethod.FastScale)
  }

  /**
   * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
   * @param yearlyData Sequence of (year, data), where `data` is some data associated with
   *                   `year`. The type of `data` can be anything.
   * @param generateImage Function that generates an image given a year, a zoom level, the x and
   *                      y coordinates of the tile and the data to build the image from
   */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit): Unit = {

    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until 1 << zoom
      y <- 0 until 1 << zoom
    } generateImage(year, Tile(x, y, zoom), data)

  }

}
  