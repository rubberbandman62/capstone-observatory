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
    val p = 2
    val (n, d) = locationsTemperatures.par.aggregate((0.0d, 0.0d))((acc, locTemp) => {
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
      case ((upper, lower), (t, c)) => {
        if (abs(t - value) < 0.05) {
          ((value, c), (value, c))
        } else if (t > value && t < upper._1)
          ((t, c), lower)
        else if (t < value && t > lower._1)
          (upper, (t, c))
        else
          (upper, lower)
      }
    })

    // if value exactly matches one temperature than return the corresponding color
    if (upper._1 == lower._1)
      upper._2
    // if value greater than the largest temperature on the scala return the highest temperature 
    else if (lower._1 == Double.MinValue)
      upper._2
    // if value is lower than the lowest temperature on the scala return the lowest temperature 
    else if (upper._1 == Double.MaxValue)
      lower._2
    else {
      val d = (value - lower._1) / (upper._1 - lower._1)
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
    val pixels = new Array[Pixel](width * height)
    for (y <- 0 until height; x <- 0 until width) {
      val temp = predictTemperature(temperatures, new Location(x, y, width, height))
      val color = interpolateColor(colors, temp)
      pixels(y * width + x) = Pixel(color.red, color.green, color.blue, alpha)
    }
    Image(width, height, pixels)
  }

}