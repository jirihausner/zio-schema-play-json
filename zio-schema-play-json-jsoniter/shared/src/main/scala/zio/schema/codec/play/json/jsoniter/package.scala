package zio.schema.codec.play.json

import com.github.plokhotnyuk.jsoniter_scala.playjson.PlayJsonFormats
import play.api.libs.json._
import zio.Chunk
import zio.schema.annotation.directDynamicMapping
import zio.schema.codec.play.json
import zio.schema.{DynamicValue, Schema, StandardType}

import java.util.Base64

package object jsoniter {

  implicit val schemaJsValue: Schema[JsValue] =
    Schema.dynamicValue.transform(toJsValue, json.fromJsValue).annotate(directDynamicMapping())

  private def toJsValue(dv: DynamicValue): JsValue = dv match {
    case DynamicValue.Primitive(value, standardType) =>
      standardType.asInstanceOf[StandardType[_]] match {
        case StandardType.UnitType       => JsObject.empty
        case StandardType.StringType     => JsString(value.asInstanceOf[String])
        case StandardType.BoolType       => JsBoolean(value.asInstanceOf[Boolean])
        case StandardType.ByteType       => PlayJsonFormats.byteFormat.writes(value.asInstanceOf[Byte])
        case StandardType.ShortType      => PlayJsonFormats.shortFormat.writes(value.asInstanceOf[Short])
        case StandardType.IntType        => PlayJsonFormats.intFormat.writes(value.asInstanceOf[Int])
        case StandardType.LongType       => PlayJsonFormats.longFormat.writes(value.asInstanceOf[Long])
        case StandardType.FloatType      => PlayJsonFormats.floatFormat.writes(value.asInstanceOf[Float])
        case StandardType.DoubleType     => PlayJsonFormats.doubleFormat.writes(value.asInstanceOf[Double])
        case StandardType.BinaryType     =>
          JsString(Base64.getEncoder.encodeToString(value.asInstanceOf[Chunk[Byte]].toArray))
        case StandardType.CharType       => JsString(value.asInstanceOf[Char].toString)
        case StandardType.UUIDType       => JsString(value.asInstanceOf[java.util.UUID].toString)
        case StandardType.BigDecimalType =>
          PlayJsonFormats.bigDecimalFormat.writes(BigDecimal(value.asInstanceOf[java.math.BigDecimal]))
        case StandardType.BigIntegerType =>
          PlayJsonFormats.bigIntFormat.writes(BigInt(value.asInstanceOf[java.math.BigInteger]))
        case StandardType.DayOfWeekType  => JsString(value.asInstanceOf[java.time.DayOfWeek].toString)
        case StandardType.MonthType      => JsString(value.asInstanceOf[java.time.Month].toString)
        case StandardType.MonthDayType  => PlayJsonFormats.monthDayFormat.writes(value.asInstanceOf[java.time.MonthDay])
        case StandardType.PeriodType    => PlayJsonFormats.periodFormat.writes(value.asInstanceOf[java.time.Period])
        case StandardType.YearType      => PlayJsonFormats.yearFormat.writes(value.asInstanceOf[java.time.Year])
        case StandardType.YearMonthType =>
          PlayJsonFormats.yearMonthFormat.writes(value.asInstanceOf[java.time.YearMonth])
        case StandardType.ZoneIdType    => JsString(value.asInstanceOf[java.time.ZoneId].toString)
        case StandardType.ZoneOffsetType => JsString(value.asInstanceOf[java.time.ZoneOffset].toString)
        case StandardType.DurationType  => PlayJsonFormats.durationFormat.writes(value.asInstanceOf[java.time.Duration])
        case StandardType.InstantType   => PlayJsonFormats.instantFormat.writes(value.asInstanceOf[java.time.Instant])
        case StandardType.LocalDateType =>
          PlayJsonFormats.localDateFormat.writes(value.asInstanceOf[java.time.LocalDate])
        case StandardType.LocalTimeType =>
          PlayJsonFormats.localTimeFormat.writes(value.asInstanceOf[java.time.LocalTime])
        case StandardType.LocalDateTimeType  =>
          PlayJsonFormats.localDateTimeFormat.writes(value.asInstanceOf[java.time.LocalDateTime])
        case StandardType.OffsetTimeType     =>
          PlayJsonFormats.offsetTimeFormat.writes(value.asInstanceOf[java.time.OffsetTime])
        case StandardType.OffsetDateTimeType =>
          PlayJsonFormats.offsetDateTimeFormat.writes(value.asInstanceOf[java.time.OffsetDateTime])
        case StandardType.ZonedDateTimeType  =>
          PlayJsonFormats.zonedDateTimeFormat.writes(value.asInstanceOf[java.time.ZonedDateTime])
        case StandardType.CurrencyType       => JsString(value.asInstanceOf[java.util.Currency].toString)
      }
    case other                                       => json.toJsValue(other)
  }
}
