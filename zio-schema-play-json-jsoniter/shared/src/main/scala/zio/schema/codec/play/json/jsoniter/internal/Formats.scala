package zio.schema.codec.play.json.jsoniter.internal

import com.github.plokhotnyuk.jsoniter_scala.playjson.PlayJsonFormats
import play.api.libs.json.{Reads, Writes}
import zio.schema.StandardType

private[jsoniter] object Formats extends zio.schema.codec.play.json.internal.Formats {

  override def writesPrimitive[A](standardType: StandardType[A]): Writes[A] = standardType match {
    case StandardType.UnitType           => writesUnit
    case StandardType.StringType         => Writes.StringWrites
    case StandardType.BoolType           => Writes.BooleanWrites
    case StandardType.ByteType           => PlayJsonFormats.byteFormat
    case StandardType.ShortType          => PlayJsonFormats.shortFormat
    case StandardType.IntType            => PlayJsonFormats.intFormat
    case StandardType.LongType           => PlayJsonFormats.longFormat
    case StandardType.FloatType          => PlayJsonFormats.floatFormat
    case StandardType.DoubleType         => PlayJsonFormats.doubleFormat
    case StandardType.BinaryType         => writesChunk(PlayJsonFormats.byteFormat)
    case StandardType.CharType           => writesChar
    case StandardType.BigIntegerType     => PlayJsonFormats.bigIntFormat.contramap[java.math.BigInteger](new BigInt(_))
    case StandardType.BigDecimalType     =>
      PlayJsonFormats.bigDecimalFormat.contramap[java.math.BigDecimal](new BigDecimal(_))
    case StandardType.UUIDType           => Writes.UuidWrites
    case StandardType.DayOfWeekType      => Writes.StringWrites.contramap[java.time.DayOfWeek](_.toString)
    case StandardType.DurationType       => PlayJsonFormats.durationFormat
    case StandardType.InstantType        => PlayJsonFormats.instantFormat
    case StandardType.LocalDateType      => PlayJsonFormats.localDateFormat
    case StandardType.LocalDateTimeType  => PlayJsonFormats.localDateTimeFormat
    case StandardType.LocalTimeType      => PlayJsonFormats.localTimeFormat
    case StandardType.MonthType          => Writes.StringWrites.contramap[java.time.Month](_.toString)
    case StandardType.MonthDayType       => PlayJsonFormats.monthDayFormat
    case StandardType.OffsetDateTimeType => PlayJsonFormats.offsetDateTimeFormat
    case StandardType.OffsetTimeType     => PlayJsonFormats.offsetTimeFormat
    case StandardType.PeriodType         => PlayJsonFormats.periodFormat
    case StandardType.YearType           => PlayJsonFormats.yearFormat
    case StandardType.YearMonthType      => PlayJsonFormats.yearMonthFormat
    case StandardType.ZonedDateTimeType  => PlayJsonFormats.zonedDateTimeFormat
    case StandardType.ZoneIdType         => Writes.StringWrites.contramap[java.time.ZoneId](_.toString)
    case StandardType.ZoneOffsetType     => Writes.StringWrites.contramap[java.time.ZoneOffset](_.toString)
    case StandardType.CurrencyType       => Writes.StringWrites.contramap[java.util.Currency](_.toString)
  }

  override def readsPrimitive[A](standardType: StandardType[A]): Reads[A] = standardType match {
    case StandardType.UnitType           => readsUnit
    case StandardType.StringType         => Reads.StringReads
    case StandardType.BoolType           => Reads.BooleanReads
    case StandardType.ByteType           => PlayJsonFormats.byteFormat
    case StandardType.ShortType          => PlayJsonFormats.shortFormat
    case StandardType.IntType            => PlayJsonFormats.intFormat
    case StandardType.LongType           => PlayJsonFormats.longFormat
    case StandardType.FloatType          => PlayJsonFormats.floatFormat
    case StandardType.DoubleType         => PlayJsonFormats.doubleFormat
    case StandardType.BinaryType         => readsChunk(PlayJsonFormats.byteFormat)
    case StandardType.CharType           => readsChar
    case StandardType.BigIntegerType     => PlayJsonFormats.bigIntFormat.map(_.underlying)
    case StandardType.BigDecimalType     => PlayJsonFormats.bigDecimalFormat.map(_.underlying)
    case StandardType.UUIDType           => readsUUID
    case StandardType.DayOfWeekType      => readsJavaTime(java.time.DayOfWeek.valueOf)
    case StandardType.DurationType       => PlayJsonFormats.durationFormat
    case StandardType.InstantType        => PlayJsonFormats.instantFormat
    case StandardType.LocalDateType      => PlayJsonFormats.localDateFormat
    case StandardType.LocalDateTimeType  => PlayJsonFormats.localDateTimeFormat
    case StandardType.LocalTimeType      => PlayJsonFormats.localTimeFormat
    case StandardType.MonthType          => readsJavaTime(java.time.Month.valueOf)
    case StandardType.MonthDayType       => PlayJsonFormats.monthDayFormat
    case StandardType.OffsetDateTimeType => PlayJsonFormats.offsetDateTimeFormat
    case StandardType.OffsetTimeType     => PlayJsonFormats.offsetTimeFormat
    case StandardType.PeriodType         => PlayJsonFormats.periodFormat
    case StandardType.YearType           => PlayJsonFormats.yearFormat
    case StandardType.YearMonthType      => PlayJsonFormats.yearMonthFormat
    case StandardType.ZonedDateTimeType  => PlayJsonFormats.zonedDateTimeFormat
    case StandardType.ZoneIdType         => readsJavaTime(java.time.ZoneId.of)
    case StandardType.ZoneOffsetType     => readsJavaTime(java.time.ZoneOffset.of)
    case StandardType.CurrencyType       => readsCurrency
  }
}
