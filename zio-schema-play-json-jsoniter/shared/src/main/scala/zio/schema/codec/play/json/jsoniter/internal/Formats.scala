package zio.schema.codec.play.json.jsoniter.internal

import com.evolution.playjson.jsoniter.{PlayJsonJsoniter => PlayJsonFormats}
import play.api.libs.json.{Reads, Writes}
import zio.schema.StandardType

private[jsoniter] object Formats extends zio.schema.codec.play.json.internal.Formats {

  override def writesPrimitive[A](standardType: StandardType[A]): Writes[A] = standardType match {
    case StandardType.UnitType           => writesUnit
    case StandardType.StringType         => Writes.StringWrites
    case StandardType.BoolType           => Writes.BooleanWrites
    case StandardType.ByteType           => Writes.ByteWrites
    case StandardType.ShortType          => Writes.ShortWrites
    case StandardType.IntType            => Writes.IntWrites
    case StandardType.LongType           => Writes.LongWrites
    case StandardType.FloatType          => writesFloat
    case StandardType.DoubleType         => writesDouble
    case StandardType.BinaryType         => writesChunk(Writes.ByteWrites)
    case StandardType.CharType           => writesChar
    case StandardType.BigIntegerType     => Writes.BigIntegerWrites
    case StandardType.BigDecimalType     => Writes.BigDecimalWrites.contramap(new BigDecimal(_))
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
    case StandardType.YearType           => writesYear
    case StandardType.YearMonthType      => writesYearMonth
    case StandardType.ZonedDateTimeType  => PlayJsonFormats.zonedDateTimeFormat
    case StandardType.ZoneIdType         => Writes.StringWrites.contramap[java.time.ZoneId](_.toString)
    case StandardType.ZoneOffsetType     => Writes.StringWrites.contramap[java.time.ZoneOffset](_.toString)
    case StandardType.CurrencyType       => Writes.StringWrites.contramap[java.util.Currency](_.toString)
  }

  override def readsPrimitive[A](standardType: StandardType[A]): Reads[A] = standardType match {
    case StandardType.UnitType           => readsUnit
    case StandardType.StringType         => Reads.StringReads
    case StandardType.BoolType           => Reads.BooleanReads
    case StandardType.ByteType           => Reads.ByteReads
    case StandardType.ShortType          => Reads.ShortReads
    case StandardType.IntType            => Reads.IntReads
    case StandardType.LongType           => Reads.LongReads
    case StandardType.FloatType          => Reads.FloatReads
    case StandardType.DoubleType         => Reads.DoubleReads
    case StandardType.BinaryType         => readsChunk(Reads.ByteReads)
    case StandardType.CharType           => readsChar
    case StandardType.BigIntegerType     => Reads.BigIntegerReads
    case StandardType.BigDecimalType     => Reads.bigDecReads.map(_.bigDecimal)
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
    case StandardType.YearType           => readsJavaTime(java.time.Year.parse)
    case StandardType.YearMonthType      => readsJavaTime(java.time.YearMonth.parse)
    case StandardType.ZonedDateTimeType  => PlayJsonFormats.zonedDateTimeFormat
    case StandardType.ZoneIdType         => readsJavaTime(java.time.ZoneId.of)
    case StandardType.ZoneOffsetType     => readsJavaTime(java.time.ZoneOffset.of)
    case StandardType.CurrencyType       => readsCurrency
  }
}
