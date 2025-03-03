package zio.schema.codec.play

import play.api.libs.json._
import zio.Chunk
import zio.schema.annotation.directDynamicMapping
import zio.schema.{DynamicValue, Schema, StandardType, TypeId}

import java.util.Base64
import scala.collection.immutable.ListMap

package object json {

  implicit val schemaJsValue: Schema[JsValue] =
    Schema.dynamicValue.transform(toJsValue, fromJsValue).annotate(directDynamicMapping())

  private[play] def toJsValue(dv: DynamicValue): JsValue =
    dv match {
      case DynamicValue.Record(_, values)              => JsObject(values.map { case (k, v) => (k, toJsValue(v)) })
      case DynamicValue.Enumeration(_, _)              =>
        throw new Exception("DynamicValue.Enumeration is not supported")
      case DynamicValue.Sequence(values)               => JsArray(values.map(toJsValue).toArray)
      case DynamicValue.Dictionary(_)                  =>
        throw new Exception("DynamicValue.Dictionary is not supported")
      case DynamicValue.SetValue(values)               => JsArray(values.map(toJsValue).toArray)
      case DynamicValue.Primitive(value, standardType) =>
        standardType.asInstanceOf[StandardType[_]] match {
          case StandardType.UnitType           => JsObject.empty
          case StandardType.StringType         => JsString(value.asInstanceOf[String])
          case StandardType.BoolType           => JsBoolean(value.asInstanceOf[Boolean])
          case StandardType.ByteType           => JsNumber(value.asInstanceOf[Byte].toInt)
          case StandardType.ShortType          => JsNumber(value.asInstanceOf[Short].toInt)
          case StandardType.IntType            => JsNumber(value.asInstanceOf[Int])
          case StandardType.LongType           => JsNumber(value.asInstanceOf[Long])
          case StandardType.FloatType          => JsNumber(BigDecimal.decimal(value.asInstanceOf[Float]))
          case StandardType.DoubleType         => JsNumber(BigDecimal.decimal(value.asInstanceOf[Double]))
          case StandardType.BinaryType         =>
            JsString(Base64.getEncoder.encodeToString(value.asInstanceOf[Chunk[Byte]].toArray))
          case StandardType.CharType           => JsString(value.asInstanceOf[Char].toString)
          case StandardType.UUIDType           => JsString(value.asInstanceOf[java.util.UUID].toString)
          case StandardType.BigDecimalType     => JsNumber(BigDecimal(value.asInstanceOf[java.math.BigDecimal]))
          case StandardType.BigIntegerType     => JsNumber(BigDecimal(value.asInstanceOf[java.math.BigInteger]))
          case StandardType.DayOfWeekType      => JsString(value.asInstanceOf[java.time.DayOfWeek].toString)
          case StandardType.MonthType          => JsString(value.asInstanceOf[java.time.Month].toString)
          case StandardType.MonthDayType       => JsString(value.asInstanceOf[java.time.MonthDay].toString)
          case StandardType.PeriodType         => JsString(value.asInstanceOf[java.time.Period].toString)
          case StandardType.YearType           => JsNumber(value.asInstanceOf[java.time.Year].getValue)
          case StandardType.YearMonthType      => JsString(value.asInstanceOf[java.time.YearMonth].toString)
          case StandardType.ZoneIdType         => JsString(value.asInstanceOf[java.time.ZoneId].toString)
          case StandardType.ZoneOffsetType     => JsString(value.asInstanceOf[java.time.ZoneOffset].toString)
          case StandardType.DurationType       => JsString(value.asInstanceOf[java.time.Duration].toString)
          case StandardType.InstantType        => JsString(value.asInstanceOf[java.time.Instant].toString)
          case StandardType.LocalDateType      => JsString(value.asInstanceOf[java.time.LocalDate].toString)
          case StandardType.LocalTimeType      => JsString(value.asInstanceOf[java.time.LocalTime].toString)
          case StandardType.LocalDateTimeType  => JsString(value.asInstanceOf[java.time.LocalDateTime].toString)
          case StandardType.OffsetTimeType     => JsString(value.asInstanceOf[java.time.OffsetTime].toString)
          case StandardType.OffsetDateTimeType => JsString(value.asInstanceOf[java.time.OffsetDateTime].toString)
          case StandardType.ZonedDateTimeType  => JsString(value.asInstanceOf[java.time.ZonedDateTime].toString)
          case StandardType.CurrencyType       => JsString(value.asInstanceOf[java.util.Currency].toString)
        }
      case DynamicValue.Singleton(_)                   => JsObject.empty
      case DynamicValue.SomeValue(value)               => toJsValue(value)
      case DynamicValue.NoneValue                      => JsNull
      case DynamicValue.Tuple(left, right)             => JsArray(Array(toJsValue(left), toJsValue(right)))
      case DynamicValue.LeftValue(value)               => JsObject(Map("Left" -> toJsValue(value)))
      case DynamicValue.RightValue(value)              => JsObject(Map("Right" -> toJsValue(value)))
      case DynamicValue.BothValue(_, _)                => throw new Exception("DynamicValue.BothValue is not supported")
      case DynamicValue.DynamicAst(_) => throw new Exception("DynamicValue.DynamicAst is not supported")
      case DynamicValue.Error(_)      => throw new Exception("DynamicValue.Error is not supported")
    }

  private[play] def fromJsValue(value: JsValue): DynamicValue = value match {
    case JsNull               => DynamicValue.NoneValue
    case JsTrue               => DynamicValue.Primitive(true, StandardType.BoolType)
    case JsFalse              => DynamicValue.Primitive(false, StandardType.BoolType)
    case JsNumber(value)      => DynamicValue.Primitive(value.underlying, StandardType.BigDecimalType)
    case JsString(value)      => DynamicValue.Primitive(value, StandardType.StringType)
    case JsArray(values)      => DynamicValue.Sequence(Chunk.fromIterable(values.map(fromJsValue)))
    case JsObject(underlying) =>
      DynamicValue.Record(
        TypeId.Structural,
        ListMap(underlying.map { case (k, v) => (k, fromJsValue(v)) }.toList: _*),
      )
  }
}
