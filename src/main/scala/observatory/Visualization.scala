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
      val gcDist = location.gcDistanceTo(locTemp._1)
      val poweredDistance = pow(gcDist, p)
      val weight = if (poweredDistance == 0.00d) 10.0d else 1 / poweredDistance
//      println("in predictTemperature: " + gcDist + " / " + weight + " / " + locTemp._2) 
      (acc._1 + locTemp._2 * weight, acc._2 + weight)
    }, (acc1, acc2) => (acc1._1 + acc2._1, acc1._2 + acc2._2))

//    println("temperature: " + n + "/" + d)

    n / d
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
      val d = (value - upper._1) / (lower._1 - upper._1)
      upper._2.interpolate(lower._2, d)
    }
  }

  def myInterpolateColor(sortedByTemperatures: Array[(Temperature, Color)], value: Temperature): Color =
    if (sortedByTemperatures.head._1 >= value)
      sortedByTemperatures.head._2
    else if (sortedByTemperatures.last._1 <= value)
      sortedByTemperatures.last._2
    else {
      val i = sortedByTemperatures.indexWhere(_._1 >= value)
      val d = (value - sortedByTemperatures(i)._1) / (sortedByTemperatures(i - 1)._1 - sortedByTemperatures(i)._1)
      sortedByTemperatures(i)._2.interpolate(sortedByTemperatures(i - 1)._2, d)
    }

  /**
   * @param temperatures Known temperatures
   * @param colors Color scale
   * @return A 360×180 image where each pixel shows the predicted temperature at its location
   */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val pixels = for {
      y <- Range(0, 179, 2)
      x <- Range(0, 359, 2)
    } yield {
      val temp = predictTemperature(temperatures, new Location(x, y, 360, 180))
      val color = interpolateColor(colors, temp)
      Pixel(color.red, color.green, color.blue, 255)
    }
    Image(180, 90, pixels.toArray).scale(2, ScaleMethod.FastScale)
  }

}