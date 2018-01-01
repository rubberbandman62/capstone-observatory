package observatory

import org.apache.log4j.{ Level, Logger }
import org.apache.spark
import spark.sql._

object SparkInitialization {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val sparkSession = SparkSession
    .builder()
    .appName("Temperatures")
    .config("spark.master", "local")
    .getOrCreate()

}