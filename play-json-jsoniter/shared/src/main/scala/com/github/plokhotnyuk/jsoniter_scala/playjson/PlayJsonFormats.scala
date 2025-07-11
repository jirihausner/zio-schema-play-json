package com.github.plokhotnyuk.jsoniter_scala.playjson

import com.github.plokhotnyuk.jsoniter_scala.core._
import play.api.libs.json._

import java.math.RoundingMode
import java.time._
import scala.util.control.NonFatal

/**
 * Implicit instances of Play JSON's format for numeric and `java.time.*` types.
 *
 * Uses jsoniter-scala for efficient encoding and decoding.
 */
object PlayJsonFormats {

  private[playjson] val pool         = new ThreadLocal[(JsonReader, Array[Byte], JsonWriter)] {
    override def initialValue(): (JsonReader, Array[Byte], JsonWriter) = {
      val buf = new Array[Byte](512) // should be enough for the longest number or zoned date time value
      new Tuple3(new JsonReader(buf, charBuf = new Array[Char](512)), buf, new JsonWriter(buf))
    }
  }
  private[playjson] val readerConfig = ReaderConfig
    .withAppendHexDumpToParseException(false)
    .withPreferredBufSize(512)
    .withMaxBufSize(512)
    .withPreferredCharBufSize(512)
    .withMaxCharBufSize(512)

  private[playjson] val writeConfig = WriterConfig.withPreferredBufSize(512)

  private[this] abstract class ShortAsciiStringFormat[A](name: String) extends JsonValueCodec[A] with Format[A] {

    override def writes(x: A): JsValue = {
      val (_, buf, writer) = pool.get
      val len              = writer.write(this, x, buf, 0, 512, writeConfig)
      new JsString(StringUtil.toString(buf, len))
    }

    override def reads(json: JsValue): JsResult[A] = {
      val (reader, buf, _) = pool.get
      val s                = json.asInstanceOf[JsString].value
      var len              = 0
      if (
        (s ne null) && {
          len = s.length
          len <= 510
        } && {
          buf(0) = '"'
          var bits, i = 0
          while (i < len) {
            val ch = s.charAt(i)
            buf(i + 1) = ch.toByte
            bits |= ch
            i += 1
          }
          buf(i + 1) = '"'
          bits < 0x80
        }
      ) {
        try return JsSuccess(reader.read(this, buf, 0, len + 2, readerConfig))
        catch { case NonFatal(_) => JsError(s"error.expected.$name") }
      }
      JsError("error.expected.jsstring")
    }

    @inline
    override def nullValue: A = null.asInstanceOf[A]
  }

  private[this] abstract class NumericFormat[A](name: String) extends JsonValueCodec[A] with Format[A] {

    def readsNumber(number: JsNumber): JsResult[A]

    final def reads(json: JsValue): JsResult[A] = json match {
      case number: JsNumber => readsNumber(number)
      case JsString(value)  =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _                => fail()
    }

    @inline
    protected def fromString(value: String): A = {
      val (reader, _, _) = pool.get
      reader.read(this, value, readerConfig)
    }

    @inline
    protected def fail(): JsResult[A] = JsError(s"error.expected.$name")
  }

  // numeric formats

  implicit val byteFormat: Format[Byte] = new NumericFormat[Byte]("byte") {

    @inline
    override def decodeValue(in: JsonReader, default: Byte): Byte = {
      val x = in.readByte(isToken = false)
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: Byte, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: Byte = 0

    @inline
    final def writes(x: Byte): JsValue = JsNumber(BigDecimal.valueOf(x.toLong))

    @inline
    override def readsNumber(number: JsNumber): JsResult[Byte] = {
      val bd = intValueExact(number.value.underlying)
      if (bd ne null) {
        val l = bd.intValue
        val b = l.toByte
        if (b == l) return JsSuccess(b)
      }
      fail()
    }
  }

  implicit val shortFormat: Format[Short] = new NumericFormat[Short]("short") {

    @inline
    override def decodeValue(in: JsonReader, default: Short): Short = {
      val x = in.readShort(isToken = false)
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: Short, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: Short = 0

    @inline
    final def writes(x: Short): JsValue = JsNumber(BigDecimal.valueOf(x.toLong))

    @inline
    override def readsNumber(number: JsNumber): JsResult[Short] = {
      val bd = intValueExact(number.value.underlying)
      if (bd ne null) {
        val l = bd.intValue
        val s = l.toShort
        if (s == l) return JsSuccess(s)
      }
      fail()
    }
  }

  implicit val intFormat: Format[Int] = new NumericFormat[Int]("int") {

    @inline
    override def decodeValue(in: JsonReader, default: Int): Int = {
      val x = in.readInt(isToken = false)
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: Int, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: Int = 0

    @inline
    final def writes(x: Int): JsValue = JsNumber(BigDecimal.valueOf(x.toLong))

    @inline
    override def readsNumber(number: JsNumber): JsResult[Int] = {
      val bd = intValueExact(number.value.underlying)
      if (bd ne null) return JsSuccess(bd.intValue)
      fail()
    }
  }

  implicit val longFormat: Format[Long] = new NumericFormat[Long]("long") {

    @inline
    override def decodeValue(in: JsonReader, default: Long): Long = {
      val x = in.readLong(isToken = false)
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: Long, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: Long = 0L

    @inline
    final def writes(x: Long): JsValue = JsNumber(BigDecimal.valueOf(x))

    @inline
    override def readsNumber(number: JsNumber): JsResult[Long] = {
      val bd = longValueExact(number.value.underlying)
      if (bd ne null) return JsSuccess(bd.longValue)
      fail()
    }
  }

  implicit val floatFormat: Format[Float] = new NumericFormat[Float]("float") {

    @inline
    override def decodeValue(in: JsonReader, default: Float): Float = {
      val x = in.readFloat(isToken = false)
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: Float, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: Float = 0.0f

    @inline
    final def writes(x: Float): JsValue = JsNumber(BigDecimal.decimal(x))

    @inline
    override def readsNumber(number: JsNumber): JsResult[Float] = JsSuccess(number.value.toFloat)
  }

  implicit val doubleFormat: Format[Double] = new NumericFormat[Double]("double") {

    @inline
    override def decodeValue(in: JsonReader, default: Double): Double = {
      val x = in.readDouble(isToken = false)
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: Double, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: Double = 0.0

    @inline
    final def writes(x: Double): JsValue = JsNumber(BigDecimal.decimal(x))

    @inline
    override def readsNumber(number: JsNumber): JsResult[Double] = JsSuccess(number.value.toDouble)
  }

  implicit val bigIntFormat: Format[BigInt] = new NumericFormat[BigInt]("bigint") {

    @inline
    override def decodeValue(in: JsonReader, default: BigInt): BigInt = {
      val x = in.readBigInt(isToken = false, default, JsonReader.bigIntDigitsLimit)
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: BigInt, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: BigInt = null

    @inline
    final def writes(x: BigInt): JsValue = {
      if (x.isValidLong) JsNumber(BigDecimal.valueOf(x.longValue))
      else JsNumber(new BigDecimal(new java.math.BigDecimal(x.bigInteger)))
    }

    @inline
    override def readsNumber(number: JsNumber): JsResult[BigInt] = {
      try
        JsSuccess(
          new BigInt({
            val bd = number.value.underlying
            if (bd.scale == 0) bd.unscaledValue
            else bd.toBigIntegerExact
          }),
        )
      catch { case e if NonFatal(e) => fail() }
    }
  }

  implicit val bigDecimalFormat: Format[BigDecimal] = new NumericFormat[BigDecimal]("bigint") {

    @inline
    override def decodeValue(in: JsonReader, default: BigDecimal): BigDecimal = {
      val x = in.readBigDecimal(
        isToken = false,
        default,
        JsonReader.bigDecimalMathContext,
        JsonReader.bigDecimalScaleLimit,
        JsonReader.bigDecimalDigitsLimit,
      )
      if (in.hasRemaining()) in.decodeError("error.expected.jsstring")
      x
    }

    @inline
    override def encodeValue(x: BigDecimal, out: JsonWriter): Unit = out.writeVal(x)

    @inline
    override def nullValue: BigDecimal = null

    @inline
    final def writes(x: BigDecimal): JsValue = JsNumber(x)

    @inline
    override def readsNumber(number: JsNumber): JsResult[BigDecimal] =
      JsSuccess(new BigDecimal(number.value.underlying, JsonReader.bigDecimalMathContext))
  }

  // java.time.* formats

  implicit val durationFormat: Format[Duration] = new ShortAsciiStringFormat[Duration]("duration") {

    @inline
    override def decodeValue(in: JsonReader, default: Duration): Duration = in.readDuration(default)

    @inline
    override def encodeValue(x: Duration, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val instantFormat: Format[Instant] = new ShortAsciiStringFormat[Instant]("instant") {

    @inline
    override def decodeValue(in: JsonReader, default: Instant): Instant = in.readInstant(default)

    @inline
    override def encodeValue(x: Instant, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val localDateFormat: Format[LocalDate] = new ShortAsciiStringFormat[LocalDate]("localdate") {

    @inline
    override def decodeValue(in: JsonReader, default: LocalDate): LocalDate = in.readLocalDate(default)

    @inline
    override def encodeValue(x: LocalDate, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val localDateTimeFormat: Format[LocalDateTime] = new ShortAsciiStringFormat[LocalDateTime]("localdatetime") {

    @inline
    override def decodeValue(in: JsonReader, default: LocalDateTime): LocalDateTime = in.readLocalDateTime(default)

    @inline
    override def encodeValue(x: LocalDateTime, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val localTimeFormat: Format[LocalTime] = new ShortAsciiStringFormat[LocalTime]("localtime") {

    @inline
    override def decodeValue(in: JsonReader, default: LocalTime): LocalTime = in.readLocalTime(default)

    @inline
    override def encodeValue(x: LocalTime, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val monthDayFormat: Format[MonthDay] = new ShortAsciiStringFormat[MonthDay]("monthday") {

    @inline
    override def decodeValue(in: JsonReader, default: MonthDay): MonthDay = in.readMonthDay(default)

    @inline
    override def encodeValue(x: MonthDay, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val offsetDateTimeFormat: Format[OffsetDateTime] =
    new ShortAsciiStringFormat[OffsetDateTime]("offsetdatetime") {

      @inline
      override def decodeValue(in: JsonReader, default: OffsetDateTime): OffsetDateTime = in.readOffsetDateTime(default)

      @inline
      override def encodeValue(x: OffsetDateTime, out: JsonWriter): Unit = out.writeVal(x)
    }

  implicit val offsetTimeFormat: Format[OffsetTime] = new ShortAsciiStringFormat[OffsetTime]("offsettime") {

    @inline
    override def decodeValue(in: JsonReader, default: OffsetTime): OffsetTime = in.readOffsetTime(default)

    @inline
    override def encodeValue(x: OffsetTime, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val periodFormat: Format[Period] = new ShortAsciiStringFormat[Period]("period") {

    @inline
    override def decodeValue(in: JsonReader, default: Period): Period = in.readPeriod(default)

    @inline
    override def encodeValue(x: Period, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val yearMonthFormat: Format[YearMonth] = new ShortAsciiStringFormat[YearMonth]("yearmonth") {

    @inline
    override def decodeValue(in: JsonReader, default: YearMonth): YearMonth = in.readYearMonth(default)

    @inline
    override def encodeValue(x: YearMonth, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val yearFormat: Format[Year] = new ShortAsciiStringFormat[Year]("year") {

    @inline
    override def decodeValue(in: JsonReader, default: Year): Year = in.readYear(default)

    @inline
    override def encodeValue(x: Year, out: JsonWriter): Unit = out.writeVal(x)
  }

  implicit val zonedDateTimeFormat: Format[ZonedDateTime] = new ShortAsciiStringFormat[ZonedDateTime]("zoneddatetime") {

    @inline
    override def decodeValue(in: JsonReader, default: ZonedDateTime): ZonedDateTime = in.readZonedDateTime(default)

    @inline
    override def encodeValue(x: ZonedDateTime, out: JsonWriter): Unit = out.writeVal(x)
  }

  private[this] val intMin  = java.math.BigDecimal.valueOf(Int.MinValue)
  private[this] val intMax  = java.math.BigDecimal.valueOf(Int.MaxValue)
  private[this] val longMin = java.math.BigDecimal.valueOf(Long.MinValue)
  private[this] val longMax = java.math.BigDecimal.valueOf(Long.MaxValue)

  private[this] def intValueExact(bd: java.math.BigDecimal): java.math.BigDecimal = {
    if (bd.signum != 0) {
      var p   = bd.precision
      val s   = bd.scale
      if (p <= s || p - 10 > s) return null
      val bd0 = bd.setScale(0, RoundingMode.UNNECESSARY)
      p = bd0.precision
      if (p > 10 || p == 10 && (bd0.compareTo(intMin) < 0 || bd0.compareTo(intMax) > 0)) return null
      bd0
    } else java.math.BigDecimal.ZERO
  }

  private[this] def longValueExact(bd: java.math.BigDecimal): java.math.BigDecimal = {
    if (bd.signum != 0) {
      var p   = bd.precision
      val s   = bd.scale
      if (p <= s || p - 19 > s) return null
      val bd0 = bd.setScale(0, RoundingMode.UNNECESSARY)
      p = bd0.precision
      if (p > 19 || p == 19 && (bd0.compareTo(longMin) < 0 || bd0.compareTo(longMax) > 0)) return null
      bd0
    } else java.math.BigDecimal.ZERO
  }
}
