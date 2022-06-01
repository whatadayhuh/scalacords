package geoscalatz

import java.time.ZoneId

import geoscalatz.domain.CoordinatesHash.GeoHash

import scala.util.control.Breaks._

object TimezoneIndex {
  private lazy val hashedCoordinateCount = memoryIndex.count(_ => true)

  def seek(hash: GeoHash): Seq[String] = {
    lookupTimezone(hash).map(i => tzIndex(i - 1))
  }

  private def lookupTimezone(hash: GeoHash) = {
    performSeek(hash) match {
      case Some(idx) => {
        val (min, max) = (idx, idx)
        val foundHash = memoryIndex(idx).take(5)
        println(foundHash)

        val adjustedMin = min - memoryIndex.view(min-1000, min).reverse.takeWhile(_ == foundHash).length
        val adjustedMax = max - memoryIndex.view(max, max+1000).takeWhile(_ == foundHash).length

        println(adjustedMin)
        println(adjustedMax)

        memoryIndex.slice(adjustedMin, adjustedMax).map(_.drop(5).toInt)
      }
      case None => Nil
    }


  }

  //ughhh
  private def performSeek(hash: GeoHash): Option[Int] = {
    var min = 1
    var max = hashedCoordinateCount
    var converged = false
    println(hash)

    breakable { while (true)
    {
      val mid = ((max - min) / 2) + min
      val midLine = memoryIndex(mid)

      breakable {for (i <- 0 until hash.length){
        if (midLine(i) == '-')
        {
          println("empty branch")
          return Some(mid)
        }

        if (midLine(i) > hash(i))
        {
          max = if(mid == max)  min else mid
          break
        }
        if (midLine(i) < hash(i))
        {
          min = if(mid == min) max else mid
          break
        }

        if (i == 4)
        {
          println("match")
          return Some(mid)
        }

        if (min == mid)
        {
          min = max
          break
        }
      } }

      if (min == max)
      {
        if (converged)
          break

        converged = true
      }
    }}
    println("Converged")
    None
  }

  private val memoryIndex =
    scala.io.Source.fromFile(getClass.getResource(Tokens.CoordinateIndex).getPath)(scala.io.Codec.UTF8)
      .getLines().toIndexedSeq
      //.map(s => (s.take(5), s.drop(5).toInt)).toMap

  private val tzIndex =
    scala.io.Source.fromFile(getClass.getResource(Tokens.CoordinateIndex).getPath)(scala.io.Codec.UTF8)
      .getLines().toIndexedSeq

}
