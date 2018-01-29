package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ModelTest extends FunSuite {
  
  test("ModelTest#1: check tiles to list of Locations") {
    val tile = Tile(7, 7, 3)
    val locs = tile.toListOfLocations(256).toArray
    for (y <- 0 until 256; x <- 0 until 256) {
      val loc = locs(y*256 + x)
      if (loc.lon > 179 || loc.lon < -179 || loc.lat > 89 || loc.lat < -89) 
        println(s"($x, $y): $loc")
      if (x == y && x > 0) {
        assert(locs(y*256 + x).lat < locs((y-1)*256 + (x-1)).lat) 
        assert(locs(y*256 + x).lon > locs((y-1)*256 + (x-1)).lon) 
      }
    }
  }
  
}