package geoscalatz

import java.time.ZoneId

import geoscalatz.domain.{CoordinatesHash, Coordinates}

object GeoTZ {

  case class GeoTzResult(primary: ZoneId, alternatives: Option[Seq[ZoneId]] = None)

  def timezone(c: Coordinates): GeoTzResult = {
    println(c)
    TimezoneIndex.seek(CoordinatesHash.hash(c))
      .foldLeft(GeoTzResult(ZoneId.systemDefault()))((acc, s) => {
      acc match {
        case GeoTzResult(_, None) =>
          GeoTzResult(ZoneId.of(s), None)
        case GeoTzResult(_, None) =>
          acc.copy(alternatives = Some(ZoneId.of(s) :: Nil))
        case GeoTzResult(_, Some(h :: t)) =>
          acc.copy(alternatives = Some(ZoneId.of(s) :: h :: t))
      }
    })}

  def main(args: Array[String]): Unit = {
    println(getClass.getResource(Tokens.CoordinateIndex).getPath)
    val c = Coordinates(args(0).toDouble, args(1).toDouble)
    println(timezone(c))
  }
}
