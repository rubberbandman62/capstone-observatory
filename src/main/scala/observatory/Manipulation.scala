package observatory

import org.apache.spark.rdd.RDD

/**
 * 4th milestone: value-added information
 */
object Manipulation {

  /**
   * @param temperatures Known temperatures
   * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
   *         returns the predicted temperature at this location
   */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature =
    CachedGrid(temperatures).get _
//    ArrayGrid(temperatures).get _
//    MapGrid(temperatures).get _

  /**
   * @param temperaturess Sequence of known temperatures over the years (each element of the collection
   *                      is a collection of pairs of location and temperature)
   * @return A function that, given a latitude and a longitude, returns the average temperature at this location
   */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val grids = temperaturess.map(makeGrid(_))
    gl => grids.map(_(gl)).sum / temperaturess.size
  }
//    gl => temperaturess.map(CachedGrid(_)).foldLeft(0.0)((acc, cg) => acc + cg.get(gl)) / temperaturess.size
//    calculateAverageGrid(temperaturess).get _
  
//  def calculateAverageGrid(temperaturess: Iterable[Iterable[(Location, Temperature)]]): ArrayGrid = 
//    temperaturess.map(ArrayGrid(_)).foldLeft(ArrayGrid())(_ + _) / temperaturess.size
//  def calculateAverageGrid(temperaturess: Iterable[Iterable[(Location, Temperature)]]): MapGrid = 
//    temperaturess.map(MapGrid(_)).foldLeft(MapGrid.empty)(_ + _) / temperaturess.size

  /**
   * @param temperatures Known temperatures
   * @param normals A grid containing the “normal” temperatures
   * @return A grid containing the deviations compared to the normal temperatures
   */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)
    gl => grid(gl) - normals(gl)
  }

}

