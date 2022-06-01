package geoscalatz.domain

import scala.collection.mutable.ArrayBuffer

case class Coordinates(lattitude: Double, longitude: Double)

object  CoordinatesHash {
  type GeoHash = String


  private val chars = "0123456789bcdefghjkmnpqrstuvwxyz"
  private val hashBits = List(16, 8, 4, 2, 1)
  private val maxLen = 5

  //A pretty cool zig-zag encoding of geospatial coordinates with 8 digits of precision
 def hash(c: Coordinates): GeoHash = {
    var localHash =  List[Char]()
    var even = true

    val lats = ArrayBuffer(-90.0d, 90.0d)
    val longs = ArrayBuffer(-180.0d, 180.0d)

    while(localHash.length < maxLen) {
      val ch = hashBits.foldLeft((0, true))((acc, i) => {
        val (ch, even) = acc
        even match {
          case true =>
            println(longs)
            advance(longs, c.longitude, i, ch, even)
          case false =>
            println(lats)
            advance(lats, c.lattitude, i, ch, even)
        }
      })

      even = ch._2
      localHash = chars(ch._1) :: localHash
    }
      localHash.mkString
  }

  def average(ls: Seq[Double]): Double =
    (ls sum) / ls.length

  def advance(ls: ArrayBuffer[Double], a: Double,  bit: Int, ch: Int, even: Boolean): (Int, Boolean) = {
    val mid = average(ls)
    if(a > mid) {
      ls(0) = mid
      (ch | bit, !even)
    } else {
      ls(1) = mid
      (ch, !even)
    }


  }

}
