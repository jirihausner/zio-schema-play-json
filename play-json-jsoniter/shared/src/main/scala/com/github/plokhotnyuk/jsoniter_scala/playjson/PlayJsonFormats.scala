package com.github.plokhotnyuk.jsoniter_scala.playjson

import com.github.plokhotnyuk.jsoniter_scala.core._
import play.api.libs.json._

import java.time._

/**
 * Implicit instances of Play JSON's format for numeric and `java.time.*` types.
 *
 * Uses jsoniter-scala for efficient encoding and decoding.
 */
object PlayJsonFormats {

  private[this] val pool         = new ThreadLocal[(JsonReader, Array[Byte], JsonWriter)] {
    override def initialValue(): (JsonReader, Array[Byte], JsonWriter) = {
      val buf = new Array[Byte](512) // should be enough for the longest number or zoned date time value
      new Tuple3(new JsonReader(buf, charBuf = new Array[Char](512)), buf, new JsonWriter(buf))
    }
  }
  private[this] val readerConfig = ReaderConfig
    .withAppendHexDumpToParseException(false)
    .withPreferredBufSize(512)
    .withMaxBufSize(512)
    .withPreferredCharBufSize(512)
    .withMaxCharBufSize(512)
  private[this] val writeConfig  = WriterConfig.withPreferredBufSize(512)

  private[this] class ShortAsciiStringCodec[A](codec: JsonValueCodec[A], name: String) extends Format[A] {

    override def writes(x: A): JsValue = {
      val tlb = pool.get
      val buf = tlb._2
      JsoniterScalaCodec.asciiStringToJString(buf, tlb._3.write(codec, x, buf, 0, 512, writeConfig))
    }

    override def reads(json: JsValue): JsResult[A] = {
      val tlb = pool.get
      val buf = tlb._2
      val s   = JsoniterScalaCodec.stringValue(json)
      var len = 0
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
        try return JsSuccess(tlb._1.read(codec, buf, 0, len + 2, readerConfig))
        catch { case _: JsonReaderException => }
      }
      fail()
    }

    @inline
    private[this] def fail(): JsResult[A] = JsError(s"error.expected.$name")
  }

  implicit val byteFormat: Format[Byte] = JsoniterScalaCodec.byteFormat {
    val codec: JsonValueCodec[Byte] = new JsonValueCodec[Byte] {
      @inline
      override def decodeValue(in: JsonReader, default: Byte): Byte = {
        val x = in.readByte(isToken = false)
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: Byte, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Byte = 0
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val shortFormat: Format[Short] = JsoniterScalaCodec.shortFormat {
    val codec: JsonValueCodec[Short] = new JsonValueCodec[Short] {
      @inline
      override def decodeValue(in: JsonReader, default: Short): Short = {
        val x = in.readShort(isToken = false)
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: Short, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Short = 0
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val intFormat: Format[Int] = JsoniterScalaCodec.intFormat {
    val codec: JsonValueCodec[Int] = new JsonValueCodec[Int] {
      @inline
      override def decodeValue(in: JsonReader, default: Int): Int = {
        val x = in.readInt(isToken = false)
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: Int, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Int = 0
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val longFormat: Format[Long] = JsoniterScalaCodec.longFormat {
    val codec: JsonValueCodec[Long] = new JsonValueCodec[Long] {
      @inline
      override def decodeValue(in: JsonReader, default: Long): Long = {
        val x = in.readLong(isToken = false)
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: Long, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Long = 0L
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val floatFormat: Format[Float] = JsoniterScalaCodec.floatFormat {
    val codec: JsonValueCodec[Float] = new JsonValueCodec[Float] {
      @inline
      override def decodeValue(in: JsonReader, default: Float): Float = {
        val x = in.readFloat(isToken = false)
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: Float, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Float = 0.0f
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val doubleFormat: Format[Double] = JsoniterScalaCodec.doubleFormat {
    val codec: JsonValueCodec[Double] = new JsonValueCodec[Double] {
      @inline
      override def decodeValue(in: JsonReader, default: Double): Double = {
        val x = in.readDouble(isToken = false)
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: Double, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Double = 0.0
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val bigIntFormat: Format[BigInt] = JsoniterScalaCodec.bigIntFormat {
    val codec: JsonValueCodec[BigInt] = new JsonValueCodec[BigInt] {
      @inline
      override def decodeValue(in: JsonReader, default: BigInt): BigInt = {
        val x = in.readBigInt(isToken = false, default, JsonReader.bigIntDigitsLimit)
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: BigInt, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: BigInt = null
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val bigDecimalFormat: Format[BigDecimal] = JsoniterScalaCodec.bigDecimalFormat {
    val codec: JsonValueCodec[BigDecimal] = new JsonValueCodec[BigDecimal] {
      @inline
      override def decodeValue(in: JsonReader, default: BigDecimal): BigDecimal = {
        val x = in.readBigDecimal(
          isToken = false,
          default,
          JsonReader.bigDecimalMathContext,
          JsonReader.bigDecimalScaleLimit,
          JsonReader.bigDecimalDigitsLimit,
        )
        if (in.hasRemaining()) in.decodeError("error.expected.jstring")
        x
      }

      @inline
      override def encodeValue(x: BigDecimal, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: BigDecimal = null
    }
    s => pool.get._1.read(codec, s, readerConfig)
  }

  implicit val durationFormat: Format[Duration] = new ShortAsciiStringCodec(
    new JsonValueCodec[Duration] {
      @inline
      override def decodeValue(in: JsonReader, default: Duration): Duration = in.readDuration(default)

      @inline
      override def encodeValue(x: Duration, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Duration = null
    },
    "duration",
  )

  implicit val instantFormat: Format[Instant] = new ShortAsciiStringCodec(
    new JsonValueCodec[Instant] {
      @inline
      override def decodeValue(in: JsonReader, default: Instant): Instant = in.readInstant(default)

      @inline
      override def encodeValue(x: Instant, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Instant = null
    },
    "instant",
  )

  implicit val localDateFormat: Format[LocalDate] = new ShortAsciiStringCodec(
    new JsonValueCodec[LocalDate] {
      @inline
      override def decodeValue(in: JsonReader, default: LocalDate): LocalDate = in.readLocalDate(default)

      @inline
      override def encodeValue(x: LocalDate, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: LocalDate = null
    },
    "localdate",
  )

  implicit val localDateTimeFormat: Format[LocalDateTime] = new ShortAsciiStringCodec(
    new JsonValueCodec[LocalDateTime] {
      @inline
      override def decodeValue(in: JsonReader, default: LocalDateTime): LocalDateTime = in.readLocalDateTime(default)

      @inline
      override def encodeValue(x: LocalDateTime, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: LocalDateTime = null
    },
    "localdatetime",
  )

  implicit val localTimeFormat: Format[LocalTime] = new ShortAsciiStringCodec(
    new JsonValueCodec[LocalTime] {
      @inline
      override def decodeValue(in: JsonReader, default: LocalTime): LocalTime = in.readLocalTime(default)

      @inline
      override def encodeValue(x: LocalTime, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: LocalTime = null
    },
    "localtime",
  )

  implicit val monthDayFormat: Format[MonthDay] = new ShortAsciiStringCodec(
    new JsonValueCodec[MonthDay] {
      @inline
      override def decodeValue(in: JsonReader, default: MonthDay): MonthDay = in.readMonthDay(default)

      @inline
      override def encodeValue(x: MonthDay, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: MonthDay = null
    },
    "monthday",
  )

  implicit val offsetDateTimeFormat: Format[OffsetDateTime] = new ShortAsciiStringCodec(
    new JsonValueCodec[OffsetDateTime] {
      @inline
      override def decodeValue(in: JsonReader, default: OffsetDateTime): OffsetDateTime = in.readOffsetDateTime(default)

      @inline
      override def encodeValue(x: OffsetDateTime, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: OffsetDateTime = null
    },
    "offsetdatetime",
  )

  implicit val offsetTimeFormat: Format[OffsetTime] = new ShortAsciiStringCodec(
    new JsonValueCodec[OffsetTime] {
      @inline
      override def decodeValue(in: JsonReader, default: OffsetTime): OffsetTime = in.readOffsetTime(default)

      @inline
      override def encodeValue(x: OffsetTime, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: OffsetTime = null
    },
    "offsettime",
  )

  implicit val periodFormat: Format[Period] = new ShortAsciiStringCodec(
    new JsonValueCodec[Period] {
      @inline
      override def decodeValue(in: JsonReader, default: Period): Period = in.readPeriod(default)

      @inline
      override def encodeValue(x: Period, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Period = null
    },
    "period",
  )

  implicit val yearMonthFormat: Format[YearMonth] = new ShortAsciiStringCodec(
    new JsonValueCodec[YearMonth] {
      @inline
      override def decodeValue(in: JsonReader, default: YearMonth): YearMonth = in.readYearMonth(default)

      @inline
      override def encodeValue(x: YearMonth, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: YearMonth = null
    },
    "yearmonth",
  )

  implicit val yearFormat: Format[Year] = new ShortAsciiStringCodec(
    new JsonValueCodec[Year] {
      @inline
      override def decodeValue(in: JsonReader, default: Year): Year = in.readYear(default)

      @inline
      override def encodeValue(x: Year, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: Year = null
    },
    "year",
  )

  implicit val zonedDateTimeFormat: Format[ZonedDateTime] = new ShortAsciiStringCodec(
    new JsonValueCodec[ZonedDateTime] {
      @inline
      override def decodeValue(in: JsonReader, default: ZonedDateTime): ZonedDateTime = in.readZonedDateTime(default)

      @inline
      override def encodeValue(x: ZonedDateTime, out: JsonWriter): Unit = out.writeVal(x)

      @inline
      override def nullValue: ZonedDateTime = null
    },
    "zoneddatetime",
  )
}
