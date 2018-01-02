package observatory

import java.time.LocalDate
import org.apache.spark
import spark.sql._
import spark.sql.types._
import spark.sql.functions._
import java.nio.file.Paths
import scala.collection.JavaConverters._

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.expressions.Aggregator
import org.apache.spark.storage.StorageLevel
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

/**
 * 1st milestone: data extraction
 */
object Extraction {

  def fsPath(file: String): String = {
    val file_name = if (file.startsWith("/")) file else "/" + file
    Paths.get(getClass.getResource(file_name).toURI()).toString()
  }

  /**
   * @param year             Year number
   * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
   * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
   * @return A sequence containing triplets (date, location, temperature)
   */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] =
    myLocateTemperatures(year, stationsFile, temperaturesFile).collect()

  def myLocateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): RDD[(LocalDate, Location, Temperature)] = {
      val rawStations = sc.textFile(fsPath(stationsFile))
      val stationsRDD = rawStations.map(line => line.split(","))
        .filter(arr => arr.length >= 4 && (arr(0).trim.length > 0 || arr(1).trim.length > 0) && arr(2).length() > 0)
        .map(arr => ((arr(0).trim, arr(1).trim), (arr(2).toDouble, arr(3).toDouble)))

      val rawTemperatures = sc.textFile(fsPath(temperaturesFile))
      val temperaturesRDD = rawTemperatures.map(line => line.split(","))
        .map(arr => ((arr(0).trim, arr(1).trim), (arr(2).toInt, arr(3).toInt, (arr(4).toDouble - 32) / 1.8d))) // .persist(StorageLevel.MEMORY_AND_DISK_SER)

      val stations = sc.broadcast(stationsRDD.collect.toMap)
      (temperaturesRDD flatMap {
        case (key, dateTemperature) =>
          stations.value.get(key).map { latLon =>
            (LocalDate.of(year, dateTemperature._1, dateTemperature._2), Location(latLon._1, latLon._2), dateTemperature._3)
          }
      }).persist(StorageLevel.MEMORY_AND_DISK)

//      stationsRDD.join(temperaturesRDD).map({
//        case (_, (coord, temp)) => (LocalDate.of(year, temp._1, temp._2), Location(coord._1, coord._2), temp._3)
//      }).persist(StorageLevel.MEMORY_AND_DISK_SER)
  }

  /**
   * @param records A sequence containing triplets (date, location, temperature)
   * @return A sequence containing, for each location, the average temperature over the year.
   */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    myLocationYearlyAverageRecords(sc.parallelize(records.toSeq)).collect()

  def myLocationYearlyAverageRecords(records: RDD[(LocalDate, Location, Temperature)]): RDD[(Location, Temperature)] = {
    records.map({ case (localdate, location, temp) => ((localdate.getYear, location), (temp, 1)) })
      .reduceByKey((value1, value2) => (value1._1 + value2._1, value1._2 + value2._2), 4)
      .map({ case (key, value) => (key._2, value._1 / value._2) }).persist(StorageLevel.MEMORY_AND_DISK_SER)
  }
}