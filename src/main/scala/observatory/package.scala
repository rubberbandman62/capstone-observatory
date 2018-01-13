import org.apache.log4j.{ Level, Logger }
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext

package object observatory {
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  lazy val conf = new SparkConf().setMaster("local[4]").setAppName("Temperatures") 
  lazy val sc = new SparkContext(conf)

  val epsilon = 0.000001d
  val earthRadius = 6371.0 // kilometers

  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  type Month = Int // Calendar month
  type Day = Int // Calendar day
  
  def toGradCelsius(f: Double): Double =
    (f - 32) / 1.8d

  def toFahrenheit(g: Double): Double =
    g * 1.8d + 32
}
