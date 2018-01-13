package observatory

import com.sksamuel.scrimage.{ Image, Pixel }

import org.apache.commons.math3.util.FastMath._
import org.apache.spark.rdd.RDD
import com.sksamuel.scrimage.ScaleMethod

/**
 * 2nd milestone: basic visualization
 */
object Visualization {

  /**
   * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
   * @param location Location where to predict the temperature
   * @return The predicted temperature at `location`
   */
  def predictTemperature(locationsTemperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val p = 4
    val (n, d) = locationsTemperatures.par.aggregate((0.0d, 0.0d))((acc, locTemp) => {
      //val gcDist = location.toPoint.haversineEarthDistance(locTemp._1.toPoint)
      //val gcDist = location.altDistanceTo(locTemp._1)
      val gcDist = location.gcDistanceTo(locTemp._1)
      val weight = if (gcDist < 1.0d) {
        1000 - (gcDist * 1000).toInt
      } else 1 / pow(gcDist, p)
      (acc._1 + locTemp._2 * weight, acc._2 + weight)
    }, (acc1, acc2) => (acc1._1 + acc2._1, acc1._2 + acc2._2))
    n / d
  }

  def predictTemperatureSlow(locationsTemperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    locationsTemperatures.find(_._1 == location)
      .map(_._2)
      .getOrElse({
        val p = 2
        val (n, d) = locationsTemperatures.map({
          case (l, t) => {
            val dist = location.gcDistanceTo(l)
            (t / pow(dist, p), 1 / pow(dist, p))
          }
        }).aggregate((0.0, 0.0))((acc, e) => (acc._1 + e._1, acc._1 + e._1), (acc, e) => (acc._1 + e._1, acc._1 + e._1))
        n / d
      })
  }

  /**
   * @param points Pairs containing a value and its associated color
   * @param value The value to interpolate
   * @return The color that corresponds to `value`, according to the color scale defined by `points`
   */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    // find the closest temperatures t0 and t1 (with colors c0 and c1) next to value
    val (upper, lower) = points.foldLeft(((Double.MaxValue, Color(0, 0, 0)), (Double.MinValue, Color(0, 0, 0))))({
      case ((upper, lower), (temperature, color)) => {
        if (temperature >= value && temperature < upper._1)
          ((temperature, color), lower)
        else if (temperature <= value && temperature > lower._1)
          (upper, (temperature, color))
        else
          (upper, lower)
      }
    })

    if (lower._1 == Double.MinValue) upper._2
    else if (upper._1 == Double.MaxValue) lower._2
    else
      lower._2.interpolate(upper._2, (value - lower._1) / (upper._1 - lower._1))
  }

  def interpolateColorSimple(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    // find the closest temperatures t0 and t1 (with colors c0 and c1) next to value
    val (upper, lower) = points.foldLeft(((Double.MaxValue, Color(0, 0, 0)), (Double.MinValue, Color(0, 0, 0))))({
      case ((upper, lower), (temperature, color)) => {
        if (temperature >= value && temperature < upper._1)
          ((temperature, color), lower)
        else if (temperature <= value && temperature > lower._1)
          (upper, (temperature, color))
        else
          (upper, lower)
      }
    })

    if (lower._1 == Double.MinValue) upper._2
    else if (upper._1 == Double.MaxValue) lower._2
    else {
      val half = (upper._1 + lower._1) / 2
      val d = if (value < half) 0 else 1
      lower._2.interpolate(upper._2, d)
    }
  }

  /**
   * @param temperatures Known temperatures
   * @param colors Color scale
   * @return A 360×180 image where each pixel shows the predicted temperature at its location
   */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val width = 360
    val height = 180
    val alpha = 255
    
    val pixels = (0 until width * height).par.map(idx => {
      val color = interpolateColor(colors, predictTemperature(temperatures, new Location(idx % width, idx / width, width, height)))
      val pixel = Pixel(color.red, color.green, color.blue, alpha)
      (idx, pixel)
    }).seq
      .sortBy(_._1)
      .map(_._2)
    
    Image(width, height, pixels.toArray)
    //    val width = 360
    //    val height = 180
    //    val alpha = 255
    //    val pixels = new Array[Pixel](width * height)
    //    for (y <- 0 until height; x <- 0 until width) {
    //      val temp = predictTemperature(temperatures, new Location(x, y, width, height))
    //      val color = interpolateColor(colors, temp)
    //      pixels(y * width + x) = Pixel(color.red, color.green, color.blue, alpha)
    //    }
    //    Image(width, height, pixels)
    //    val width = 180
    //    val height = 90
    //    val alpha = 255
    //    val pixels = new Array[Pixel](width * height)
    //    for (y <- 0 until height; x <- 0 until width) {
    //      val temp = predictTemperature(temperatures, new Location(x, y, width, height))
    //      val color = interpolateColor(colors, temp)
    //      pixels(y * width + x) = Pixel(color.red, color.green, color.blue, alpha)
    //    }
    //    Image(width, height, pixels).scale(2, ScaleMethod.Bilinear)
  }

}