package zio.schema.codec.play.json.internal

import play.api.libs.json._
import zio.prelude.NonEmptyMap
import zio.schema._
import zio.schema.annotation._
import zio.schema.codec.play.json.PlayJsonCodec
import zio.{Chunk, ChunkBuilder}

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, SignStyle}
import java.time.temporal.{ChronoField, TemporalAccessor}
import java.time.{Year, YearMonth}
import java.util
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.ListMap
import scala.collection.mutable

private[play] object Formats extends Formats

private[play] trait Formats extends PlayJsonCompat {

  private val EmptyJsonObj: JsObject = JsObject.empty

  type DiscriminatorTuple = Option[(String, String)]

  val writesUnit: Writes[Unit] = new Writes[Unit] {
    def writes(unit: Unit): JsValue = EmptyJsonObj
  }

  val readsUnit: Reads[Unit] = new Reads[Unit] {
    def reads(json: JsValue): JsResult[Unit] = json match {
      case JsObject(map) =>
        if (map.isEmpty) JsSuccess(())
        else {
          val errors = new mutable.ListBuffer[(JsPath, Seq[JsonValidationError])]
          map.foreach { case (key, _) =>
            errors += JsPath \ key -> Seq(JsonValidationError("error.path.extra"))
          }
          JsError(errors.result())
        }
      case JsArray(arr)  =>
        if (arr.isEmpty) JsSuccess(())
        else {
          val errors = new mutable.ListBuffer[(JsPath, Seq[JsonValidationError])]
          val i      = 0
          while (i < arr.size) {
            errors += JsPath \ i -> Seq(JsonValidationError("error.path.extra"))
          }
          JsError(errors.result())
        }
      case JsNull        => JsSuccess(())
      case _             =>
        JsError("error.expected.jsobjectorjsarrayorjsnull")
    }
  }

  val writesChar: Writes[Char] = new Writes[Char] {
    def writes(char: Char): JsValue = JsString(char.toString)
  }

  val readsChar: Reads[Char] = new Reads[Char] {
    def reads(json: JsValue): JsResult[Char] = json match {
      case JsString(str) if str.length == 1 => JsSuccess(str.head)
      case _                                =>
        JsError(s"error.expected.character")
    }
  }

  val writesFloat: Writes[Float] = new Writes[Float] {
    def writes(o: Float): JsValue = JsNumber(BigDecimal.decimal(o))
  }

  val writesDouble: Writes[Double] = new Writes[Double] {
    def writes(o: Double): JsValue = JsNumber(BigDecimal.decimal(o))
  }

  val readsUUID: Reads[UUID] = new Reads.UUIDReader(true)

  val writesYear: Writes[Year] = new JavaTimeWrites[Year] {
    def format: DateTimeFormatter =
      new DateTimeFormatterBuilder().appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD).toFormatter()
  }

  val writesYearMonth: Writes[YearMonth] = new JavaTimeWrites[YearMonth] {
    def format: DateTimeFormatter = new DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .toFormatter()
  }

  private[this] abstract class JavaTimeWrites[A <: TemporalAccessor] extends Writes[A] {

    protected[this] def format: DateTimeFormatter

    final def writes(a: A): JsString = JsString(format.format(a))
  }

  protected def readsJavaTime[A](f: String => A): Reads[A] = new Reads[A] {
    def reads(json: JsValue): JsResult[A] = json match {
      case JsString(str) =>
        try {
          JsSuccess(f(str))
        } catch {
          case dte: java.time.DateTimeException => JsError(s"$str is not a valid ISO-8601 format: ${dte.getMessage}")
          case ex: Exception                    =>
            JsError(ex.getMessage)
        }
      case _             =>
        JsError(s"error.expected.jsstring")
    }
  }

  val readsCurrency: Reads[java.util.Currency] = new Reads[java.util.Currency] {
    def reads(json: JsValue): JsResult[java.util.Currency] = json match {
      case JsString(str) =>
        try {
          JsSuccess(java.util.Currency.getInstance(str))
        } catch {
          case _: Throwable => JsError(s"error.expected.validcurrency")
        }
      case _             =>
        JsError(s"error.expected.jsstring")
    }
  }

  def writesChunk[A](implicit w: Writes[A]): Writes[Chunk[A]] = new Writes[Chunk[A]] {
    def writes(chunk: Chunk[A]): JsValue = {
      val builder  = mutable.ArrayBuilder.make[JsValue]
      val iterator = chunk.iterator
      while (iterator.hasNext) {
        builder += w.writes(iterator.next())
      }
      JsArray(builder.result())
    }
  }

  def readsChunk[A](implicit r: Reads[A]): Reads[Chunk[A]] = new Reads[Chunk[A]] {
    def reads(json: JsValue): JsResult[Chunk[A]] = json match {
      case JsArray(xs) =>
        val builder                        = ChunkBuilder.make[A](xs.size)
        val iterator                       = xs.iterator
        var acc: JsResult[ChunkBuilder[A]] = JsSuccess(builder)
        var i                              = 0
        while (iterator.hasNext) {
          val elem = iterator.next()
          acc = (acc, r.reads(elem)) match {
            case (JsSuccess(vs, _), JsSuccess(v, _)) => JsSuccess(vs += v)
            case (_: JsSuccess[_], jsError: JsError) => jsError.repath(JsPath(i))
            case (JsError(errors0), JsError(errors)) =>
              JsError(errors0 ++ errors.map { case (p, s) => (JsPath(i) ++ p) -> s })
            case (jsError: JsError, _: JsSuccess[_]) => jsError
          }
          i += 1
        }
        acc.map(_.result())
      case _           => JsError(JsPath, JsonValidationError("error.expected.jsarray"))
    }
  }

  def writesEither[A, B](implicit wa: Writes[A], wb: Writes[B]): OWrites[Either[A, B]] = new OWrites[Either[A, B]] {
    def writes(either: Either[A, B]): JsObject = either match {
      case Left(a)  => JsObject(Map("Left" -> wa.writes(a)))
      case Right(b) => JsObject(Map("Right" -> wb.writes(b)))
    }
  }

  def readsEither[A, B](implicit ra: Reads[A], rb: Reads[B]): Reads[Either[A, B]] = new Reads[Either[A, B]] {
    def reads(json: JsValue): JsResult[Either[A, B]] = json match {
      case obj @ JsObject(underlying) =>
        (obj \ "Left").validate[A] match {
          case JsSuccess(a, _) => JsSuccess(Left(a))
          case JsError(_)      =>
            (obj \ "Right").validate[B] match {
              case JsSuccess(b, _) => JsSuccess(Right(b))
              case JsError(errors) => JsError(errors)
            }
        }
      case _                          => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsobject"))))
    }
  }

  def writesSuspend[A](w: => Writes[A]): Writes[A] = new Writes[A] {
    lazy val underlying: Writes[A] = w
    def writes(a: A): JsValue      = underlying.writes(a)
  }

  def readsSuspend[A](r: => Reads[A]): Reads[A] = new Reads[A] {
    lazy val underlying: Reads[A]         = r
    def reads(json: JsValue): JsResult[A] = underlying.reads(json)
  }

  def readsFail[A](message: String): Reads[A] = new Reads[A] {
    def reads(json: JsValue): JsResult[A] = JsError(message)
  }

  def writesPrimitive[A](standardType: StandardType[A]): Writes[A] = standardType match {
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
    case StandardType.DurationType       => Writes.StringWrites.contramap[java.time.Duration](_.toString)
    case StandardType.InstantType        => Writes.StringWrites.contramap[java.time.Instant](_.toString)
    case StandardType.LocalDateType      => Writes.StringWrites.contramap[java.time.LocalDate](_.toString)
    case StandardType.LocalDateTimeType  => Writes.StringWrites.contramap[java.time.LocalDateTime](_.toString)
    case StandardType.LocalTimeType      => Writes.StringWrites.contramap[java.time.LocalTime](_.toString)
    case StandardType.MonthType          => Writes.StringWrites.contramap[java.time.Month](_.toString)
    case StandardType.MonthDayType       => Writes.StringWrites.contramap[java.time.MonthDay](_.toString)
    case StandardType.OffsetDateTimeType => Writes.StringWrites.contramap[java.time.OffsetDateTime](_.toString)
    case StandardType.OffsetTimeType     => Writes.StringWrites.contramap[java.time.OffsetTime](_.toString)
    case StandardType.PeriodType         => Writes.StringWrites.contramap[java.time.Period](_.toString)
    case StandardType.YearType           => writesYear
    case StandardType.YearMonthType      => writesYearMonth
    case StandardType.ZonedDateTimeType  => Writes.StringWrites.contramap[java.time.ZonedDateTime](_.toString)
    case StandardType.ZoneIdType         => Writes.StringWrites.contramap[java.time.ZoneId](_.toString)
    case StandardType.ZoneOffsetType     => Writes.StringWrites.contramap[java.time.ZoneOffset](_.toString)
    case StandardType.CurrencyType       => Writes.StringWrites.contramap[java.util.Currency](_.toString)
  }

  def readsPrimitive[A](standardType: StandardType[A]): Reads[A] = standardType match {
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
    case StandardType.DurationType       => readsJavaTime(java.time.Duration.parse)
    case StandardType.InstantType        => readsJavaTime(java.time.Instant.parse)
    case StandardType.LocalDateType      => readsJavaTime(java.time.LocalDate.parse)
    case StandardType.LocalDateTimeType  => readsJavaTime(java.time.LocalDateTime.parse)
    case StandardType.LocalTimeType      => readsJavaTime(java.time.LocalTime.parse)
    case StandardType.MonthType          => readsJavaTime(java.time.Month.valueOf)
    case StandardType.MonthDayType       => readsJavaTime(java.time.MonthDay.parse)
    case StandardType.OffsetDateTimeType => readsJavaTime(java.time.OffsetDateTime.parse)
    case StandardType.OffsetTimeType     => readsJavaTime(java.time.OffsetTime.parse)
    case StandardType.PeriodType         => readsJavaTime(java.time.Period.parse)
    case StandardType.YearType           => readsJavaTime(java.time.Year.parse)
    case StandardType.YearMonthType      => readsJavaTime(java.time.YearMonth.parse)
    case StandardType.ZonedDateTimeType  => readsJavaTime(java.time.ZonedDateTime.parse)
    case StandardType.ZoneIdType         => readsJavaTime(java.time.ZoneId.of)
    case StandardType.ZoneOffsetType     => readsJavaTime(java.time.ZoneOffset.of)
    case StandardType.CurrencyType       => readsCurrency
  }

  private case class WritesKey[A](
    schema: Schema[A],
    config: PlayJsonCodec.Configuration,
    discriminatorTuple: DiscriminatorTuple,
  ) {
    override val hashCode: Int = System.identityHashCode(schema) ^ config.hashCode ^ discriminatorTuple.hashCode
    override def equals(obj: Any): Boolean = obj match {
      case x: WritesKey[_] => (x.schema eq schema) && x.config == config && x.discriminatorTuple == discriminatorTuple
      case _               => false
    }
  }

  private case class ReadsKey[A](
    schema: Schema[A],
    config: PlayJsonCodec.Configuration,
    discriminator: Option[String],
  ) {
    override val hashCode: Int             = System.identityHashCode(schema) ^ config.hashCode ^ discriminator.hashCode
    override def equals(obj: Any): Boolean = obj match {
      case x: ReadsKey[_] => (x.schema eq schema) && x.config == config && x.discriminator == discriminator
      case _              => false
    }
  }

  private val writers = new ConcurrentHashMap[WritesKey[_], Writes[_]]()
  private val readers = new ConcurrentHashMap[ReadsKey[_], Reads[_]]()

  def writesSchema[A](
    schema: Schema[A],
    config: PlayJsonCodec.Configuration,
    discriminatorTuple: DiscriminatorTuple = None,
  ): Writes[A] = {
    val key               = WritesKey(schema, config, discriminatorTuple)
    var writes: Writes[A] = writers.get(key).asInstanceOf[Writes[A]]
    if (writes eq null) {
      writes = writesSchemaSlow(schema, config, discriminatorTuple)
      writers.put(key, writes)
    }
    writes
  }

  def readsSchema[A](
    schema: Schema[A],
    config: PlayJsonCodec.Configuration,
    discriminator: Option[String] = None,
  ): Reads[A] = {
    val key   = ReadsKey(schema, config, discriminator)
    var reads = readers.get(key).asInstanceOf[Reads[A]]
    if (reads eq null) {
      reads = readsSchemaSlow(schema, config, discriminator)
      readers.put(key, reads)
    }
    reads
  }

  def writesSchemaSlow[A](
    schema: Schema[A],
    config: PlayJsonCodec.Configuration,
    discriminatorTuple: DiscriminatorTuple = None,
  ): Writes[A] = schema match {
    case Schema.Primitive(standardType, _)           => writesPrimitive(standardType)
    case Schema.Optional(schema, _)                  => Writes.optionWithNull(writesSchema(schema, config))
    case Schema.Tuple2(l, r, _)                      => Writes.Tuple2W(writesSchema(l, config), writesSchema(r, config))
    case Schema.Sequence(schema, _, g, _, _)         => writesChunk(writesSchema(schema, config)).contramap(g)
    case Schema.NonEmptySequence(schema, _, g, _, _) => writesChunk(writesSchema(schema, config)).contramap(g)
    case Schema.Map(ks, vs, _)                       => writesMap(ks, vs, config)
    case Schema.NonEmptyMap(ks, vs, _)               => writesMap(ks, vs, config).contramap(_.toMap)
    case Schema.Set(s, _)                            => Writes.set(writesSchema(s, config))
    case Schema.Transform(c, _, g, a, _)             =>
      writesTransform(a.foldLeft(c)((s, a) => s.annotate(a)), g, config, discriminatorTuple)
    case Schema.Fail(_, _)                           => writesUnit.contramap(_ => ())
    case Schema.Either(left, right, _)      => writesEither(writesSchema(left, config), writesSchema(right, config))
    case Schema.Fallback(left, right, _, _) => writesFallback(writesSchema(left, config), writesSchema(right, config))
    case s: Schema.Lazy[A]                  => writesSuspend(writesSchema(s.schema, config, discriminatorTuple))
    case s: Schema.GenericRecord            => writesRecord(s, config, discriminatorTuple)
    case s: Schema.Record[A]                => writesCaseClass(s, config, discriminatorTuple)
    case s: Schema.Enum[A]                  => writesEnum(s, config)
    case s: Schema.Dynamic                  => writesDynamic(s, config)
    case null                               =>
      throw new Exception(s"A captured schema is null, most likely due to wrong field initialization order")
  }

  def readsSchemaSlow[A](
    schema: Schema[A],
    config: PlayJsonCodec.Configuration,
    discriminator: Option[String] = None,
  ): Reads[A] = schema match {
    case Schema.Primitive(standardType, _)  => readsPrimitive(standardType)
    case Schema.Optional(codec, _)          => Reads.optionWithNull(readsSchema(codec, config))
    case Schema.Tuple2(left, right, _)      => Reads.Tuple2R(readsSchema(left, config), readsSchema(right, config))
    case Schema.Sequence(codec, f, _, _, _) => readsChunk(readsSchema(codec, config)).map(f)
    case s @ Schema.NonEmptySequence(codec, _, _, _, _) => readsChunk(readsSchema(codec, config)).map(s.fromChunk)
    case Schema.Map(ks, vs, _)                          => readsMap(ks, vs, config)
    case Schema.NonEmptyMap(ks, vs, _)                  =>
      readsMap(ks, vs, config).flatMapResult { map =>
        NonEmptyMap.fromMapOption(map) match {
          case Some(res) => JsSuccess(res)
          case None      => JsError("Expected a non-empty map")
        }
      }
    case Schema.Set(s, _)                               => Reads.set(readsSchema(s, config))
    case Schema.Transform(c, f, _, a, _)                =>
      readsSchema(a.foldLeft(c)((s, a) => s.annotate(a)), config, discriminator).flatMapResult { a =>
        f(a) match {
          case Left(reason) => JsError(reason)
          case Right(value) => JsSuccess(value)
        }
      }
    case Schema.Fail(message, _)                        => readsFail(message)
    case Schema.Either(left, right, _)           => readsEither(readsSchema(left, config), readsSchema(right, config))
    case s @ Schema.Fallback(_, _, _, _)         => readsFallback(s, config)
    case s: Schema.Lazy[_]                       => readsSuspend(readsSchema(s.schema, config, discriminator))
    case s: Schema.GenericRecord                 => readsRecord(s, config, discriminator)
    case s @ Schema.CaseClass0(_, _, _)          => readsCaseClass0(s, config, discriminator)
    case s @ Schema.CaseClass1(_, _, _, _)       => readsCaseClass1(s, config, discriminator)
    case s @ Schema.CaseClass2(_, _, _, _, _)    => readsCaseClass2(s, config, discriminator)
    case s @ Schema.CaseClass3(_, _, _, _, _, _) => readsCaseClass3(s, config, discriminator)
    case s @ Schema.CaseClass4(_, _, _, _, _, _, _)                       => readsCaseClass4(s, config, discriminator)
    case s @ Schema.CaseClass5(_, _, _, _, _, _, _, _)                    => readsCaseClass5(s, config, discriminator)
    case s @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _)                 => readsCaseClass6(s, config, discriminator)
    case s @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _)              => readsCaseClass7(s, config, discriminator)
    case s @ Schema.CaseClass8(_, _, _, _, _, _, _, _, _, _, _)           => readsCaseClass8(s, config, discriminator)
    case s @ Schema.CaseClass9(_, _, _, _, _, _, _, _, _, _, _, _)        => readsCaseClass9(s, config, discriminator)
    case s @ Schema.CaseClass10(_, _, _, _, _, _, _, _, _, _, _, _, _)    => readsCaseClass10(s, config, discriminator)
    case s @ Schema.CaseClass11(_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
      readsCaseClass11(s, config, discriminator)
    case s @ Schema.CaseClass12(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                      =>
      readsCaseClass12(s, config, discriminator)
    case s @ Schema.CaseClass13(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                   =>
      readsCaseClass13(s, config, discriminator)
    case s @ Schema
          .CaseClass14(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
      readsCaseClass14(s, config, discriminator)
    case s @ Schema
          .CaseClass15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
      readsCaseClass15(s, config, discriminator)
    case s @ Schema.CaseClass16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)          =>
      readsCaseClass16(s, config, discriminator)
    case s @ Schema.CaseClass17(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)       =>
      readsCaseClass17(s, config, discriminator)
    case s @ Schema.CaseClass18(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)    =>
      readsCaseClass18(s, config, discriminator)
    case s @ Schema.CaseClass19(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
      readsCaseClass19(s, config, discriminator)
    case s @ Schema.CaseClass20(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
      readsCaseClass20(s, config, discriminator)
    case s @ Schema.CaseClass21(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
      readsCaseClass21(s, config, discriminator)
    case s @ Schema.CaseClass22(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
      readsCaseClass22(s, config, discriminator)
    case s: Schema.Enum[A] => readsEnum(s, config)
    case s: Schema.Dynamic => readsDynamic(s, config)
    case _                 => throw new Exception(s"Missing a handler for decoding of schema ${schema.toString()}.")
  }

  val writesStringKey: KeyWrites[String] =
    new KeyWrites[String] { def writeKey(key: String): String = key }
  val readsStringKey: KeyReads[String]   =
    new KeyReads[String] { def readKey(key: String): JsResult[String] = JsSuccess(key) }

  val writesCharKey: KeyWrites[Char] = KeyWrites.anyValKeyWrites
  val readsCharKey: KeyReads[Char]   = new KeyReads[Char] {
    def readKey(key: String): JsResult[Char] = key.toList match {
      case ch :: Nil => JsSuccess(ch)
      case _         => JsError("error.expected.character")
    }
  }

  val writesBooleanKey: KeyWrites[Boolean] = KeyWrites.anyValKeyWrites
  val readsBooleanKey: KeyReads[Boolean]   = new KeyReads[Boolean] {
    def readKey(key: String): JsResult[Boolean] = key match {
      case "true"  => JsSuccess(true)
      case "false" => JsSuccess(false)
      case _       => JsError("error.expected.boolean")
    }
  }

  val writesByteKey: KeyWrites[Byte] = KeyWrites.anyValKeyWrites
  val readsByteKey: KeyReads[Byte]   = readsCharKey.map(_.toByte)

  val writesShortKey: KeyWrites[Short] = KeyWrites.anyValKeyWrites
  val readsShortKey: KeyReads[Short]   = new KeyReads[Short] {
    def readKey(key: String): JsResult[Short] = {
      try {
        val short = key.toShort
        JsSuccess(short)
      } catch {
        case _: NumberFormatException => JsError("error.expected.short")
      }
    }
  }

  val writesIntKey: KeyWrites[Int] = KeyWrites.anyValKeyWrites
  val readsIntKey: KeyReads[Int]   = new KeyReads[Int] {
    def readKey(key: String): JsResult[Int] = {
      try {
        val int = key.toInt
        JsSuccess(int)
      } catch {
        case _: NumberFormatException => JsError("error.expected.int")
      }
    }
  }

  val writesLongKey: KeyWrites[Long] = KeyWrites.anyValKeyWrites
  val readsLongKey: KeyReads[Long]   = new KeyReads[Long] {
    def readKey(key: String): JsResult[Long] = {
      try {
        val long = key.toLong
        JsSuccess(long)
      } catch {
        case _: NumberFormatException => JsError("error.expected.long")
      }
    }
  }

  val writesUUIDKey: KeyWrites[UUID] = new KeyWrites[UUID] {
    def writeKey(key: UUID): String = key.toString()
  }
  val readsUUIDKey: KeyReads[UUID]   = new KeyReads[UUID] {
    def readKey(key: String): JsResult[UUID] = {
      try {
        JsSuccess(UUID.fromString(key))
      } catch {
        case _: IllegalArgumentException => JsError("error.expected.uuid")
      }
    }
  }

  def writesField[B](schema: Schema[B], config: PlayJsonCodec.Configuration): Option[KeyWrites[B]] = schema match {
    case Schema.Primitive(StandardType.StringType, _)                                    => Some(writesStringKey)
    case Schema.Primitive(StandardType.CharType, _)                                      => Some(writesCharKey)
    case Schema.Primitive(StandardType.BoolType, _)                                      => Some(writesBooleanKey)
    case Schema.Primitive(StandardType.ByteType, _)                                      => Some(writesByteKey)
    case Schema.Primitive(StandardType.ShortType, _)                                     => Some(writesShortKey)
    case Schema.Primitive(StandardType.IntType, _)                                       => Some(writesIntKey)
    case Schema.Primitive(StandardType.LongType, _)                                      => Some(writesLongKey)
    case Schema.Primitive(StandardType.UUIDType, _)                                      => Some(writesUUIDKey)
    case schema: Schema.Enum[_] if schema.annotations.exists(_.isInstanceOf[simpleEnum]) =>
      Some(new KeyWrites[B] { def writeKey(key: B): String = caseMap(schema, config)(key) })
    case Schema.Transform(c, _, g, a, _)                                                 =>
      writesField(a.foldLeft(c)((s, a) => s.annotate(a)), config).map { w =>
        new KeyWrites[B] {
          def writeKey(b: B): String = g(b) match {
            case Left(reason) => throw new RuntimeException(s"Failed to write key $b: $reason")
            case Right(value) => w.writeKey(value)
          }
        }
      }
    case Schema.Lazy(inner)                                                              => writesField(inner(), config)
    case _                                                                               => None
  }

  def readsField[A](schema: Schema[A], config: PlayJsonCodec.Configuration): Option[KeyReads[A]] = schema match {
    case Schema.Primitive(StandardType.StringType, _)                                    => Some(readsStringKey)
    case Schema.Primitive(StandardType.CharType, _)                                      => Some(readsCharKey)
    case Schema.Primitive(StandardType.BoolType, _)                                      => Some(readsBooleanKey)
    case Schema.Primitive(StandardType.ByteType, _)                                      => Some(readsByteKey)
    case Schema.Primitive(StandardType.ShortType, _)                                     => Some(readsShortKey)
    case Schema.Primitive(StandardType.IntType, _)                                       => Some(readsIntKey)
    case Schema.Primitive(StandardType.LongType, _)                                      => Some(readsLongKey)
    case Schema.Primitive(StandardType.UUIDType, _)                                      => Some(readsUUIDKey)
    case schema: Schema.Enum[_] if schema.annotations.exists(_.isInstanceOf[simpleEnum]) =>
      Some {
        val caseNameAliases = Formats.caseNameAliases(schema, config)

        new KeyReads[A] {

          val cases = new util.HashMap[String, A](caseNameAliases.size << 1)
          caseNameAliases.foreach { case (name, case_) =>
            cases.put(format(name, config), case_.schema.asInstanceOf[Schema.CaseClass0[A]].defaultConstruct())
          }

          def readKey(key: String): JsResult[A] = {
            if (key.exists(_.isWhitespace)) JsError(s"error.expected.validenumstring")
            else {
              val result = cases.get(key)
              if (result == null) JsError(s"error.unrecognized.subtype.$key")
              else JsSuccess(result)
            }
          }
        }
      }
    case Schema.Transform(c, f, _, a, _)                                                 =>
      readsField(a.foldLeft(c)((s, a) => s.annotate(a)), config).map { reads =>
        reads.map { key =>
          f(key) match {
            case Left(reason) => throw new RuntimeException(s"Failed to read key $a: $reason")
            case Right(a)     => a
          }
        }
      }
    case Schema.Lazy(inner)                                                              => readsField(inner(), config)
    case _                                                                               => None
  }

  def writesMap[K, V](
    ks: Schema[K],
    vs: Schema[V],
    config: PlayJsonCodec.Configuration,
  ): Writes[Map[K, V]] = writesField(ks, config) match {
    case Some(keyWrites) =>
      implicit val kw: KeyWrites[K] = keyWrites
      implicit val vw: Writes[V]    = writesSchema(vs, config)
      implicitly[Writes[Map[K, V]]]
    case None            =>
      writesChunk(Writes.Tuple2W(writesSchema(ks, config), writesSchema(vs, config)))
        .contramap(Chunk.fromIterable)
  }

  def readsMap[K, V](ks: Schema[K], vs: Schema[V], config: PlayJsonCodec.Configuration): Reads[Map[K, V]] =
    readsField(ks, config) match {
      case Some(keyReads) =>
        implicit val kr: KeyReads[K] = keyReads
        implicit val vr: Reads[V]    = readsSchema(vs, config)
        implicitly[Reads[Map[K, V]]]
      case None           =>
        readsChunk(Reads.Tuple2R(readsSchema(ks, config), readsSchema(vs, config))).map(_.toMap)
    }

  @inline
  private def isEmptyJsonArray(json: JsValue): Boolean = json match {
    case JsArray(arr) => arr.isEmpty
    case _            => false
  }

  def writesDynamic(schema: Schema.Dynamic, config: PlayJsonCodec.Configuration): Writes[DynamicValue] = {
    if (schema.annotations.exists(_.isInstanceOf[directDynamicMapping])) {
      new Writes[DynamicValue] { writes =>
        def writes(a: DynamicValue): JsValue = a match {
          case DynamicValue.Record(_, values)              =>
            val fields = values.map { case (k, v) =>
              val json = writes.writes(v)
              if (
                (config.explicitEmptyCollections.encoding || !isEmptyJsonArray(json)) &&
                (config.explicitNullValues.encoding || json != JsNull)
              ) Some(k -> json)
              else None
            }
            JsObject(fields.collect { case Some(kv) => kv }.toMap)
          case DynamicValue.Enumeration(_, _)              =>
            throw new Exception(s"DynamicValue.Enumeration is not supported in directDynamicMapping mode")
          case DynamicValue.Sequence(values)               =>
            val maybeValues = values.map { case v =>
              val json = writes.writes(v)
              if (
                (config.explicitEmptyCollections.encoding || !isEmptyJsonArray(json)) &&
                (config.explicitNullValues.encoding || json != JsNull)
              ) Some(json)
              else None
            }
            JsArray(maybeValues.flatten)
          case DynamicValue.Dictionary(_)                  =>
            throw new Exception(s"DynamicValue.Dictionary is not supported in directDynamicMapping mode")
          case DynamicValue.SetValue(values)               =>
            val maybeValues = Chunk.fromIterable(values).map { case v =>
              val json = writes.writes(v)
              if (
                (config.explicitEmptyCollections.encoding || !isEmptyJsonArray(json)) &&
                (config.explicitNullValues.encoding || json != JsNull)
              ) Some(json)
              else None
            }
            JsArray(maybeValues.flatten)
          case DynamicValue.Primitive(value, standardType) => writesPrimitive(standardType).writes(value)
          case DynamicValue.Singleton(_)                   => EmptyJsonObj
          case DynamicValue.SomeValue(value)               => writes.writes(value)
          case DynamicValue.NoneValue                      => JsNull
          case DynamicValue.Tuple(_, _)                    =>
            throw new Exception(s"DynamicValue.Tuple is not supported in directDynamicMapping mode")
          case DynamicValue.LeftValue(_)                   =>
            throw new Exception(s"DynamicValue.LeftValue is not supported in directDynamicMapping mode")
          case DynamicValue.RightValue(_)                  =>
            throw new Exception(s"DynamicValue.RightValue is not supported in directDynamicMapping mode")
          case DynamicValue.BothValue(left, right)         =>
            Writes.Tuple2W(writes, writes).writes((left, right))
          case DynamicValue.DynamicAst(_)                  =>
            throw new Exception(s"DynamicValue.DynamicAst is not supported in directDynamicMapping mode")
          case DynamicValue.Error(message)                 =>
            throw new Exception(message)
        }
      }
    } else writesSchema(DynamicValue.schema, config)
  }

  def readsDynamic(schema: Schema.Dynamic, config: PlayJsonCodec.Configuration): Reads[DynamicValue] = {
    val directMapping = schema.annotations.exists(_.isInstanceOf[directDynamicMapping])
    if (directMapping) Reads.JsValueReads.map(zio.schema.codec.play.json.fromJsValue)
    else readsSchema(DynamicValue.schema, config)
  }

  def writesTransform[A, B](
    schema: Schema[A],
    g: B => Either[String, A],
    config: PlayJsonCodec.Configuration,
    discriminatorTuple: DiscriminatorTuple,
  ): Writes[B] = new Writes[B] {
    def writes(b: B): JsValue = g(b) match {
      case Left(_)  => JsNull
      case Right(a) => writesSchema(schema, config, discriminatorTuple).writes(a)
    }
  }

  private def format(caseName: String, config: PlayJsonCodec.Configuration): String =
    if (config.discriminatorFormat == NameFormat.Identity) caseName
    else config.discriminatorFormat(caseName)

  protected def caseNameAliases[Z](parentSchema: Schema.Enum[Z], config: PlayJsonCodec.Configuration) = {
    val caseNameAliases = new mutable.HashMap[String, Schema.Case[Z, Any]]
    parentSchema.cases.foreach { case_ =>
      val schema = case_.asInstanceOf[Schema.Case[Z, Any]]
      caseNameAliases.put(format(case_.caseName, config), schema)
      case_.caseNameAliases.foreach(a => caseNameAliases.put(a, schema))
    }
    caseNameAliases
  }

  private def caseMap[Z](schema: Schema.Enum[Z], config: PlayJsonCodec.Configuration): Map[Z, String] =
    schema.nonTransientCases
      .map(case_ =>
        case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct() ->
          format(case_.caseName, config),
      )
      .toMap

  def writesEnum[Z](schema: Schema.Enum[Z], config: PlayJsonCodec.Configuration): Writes[Z] = {
    // if all cases are CaseClass0, encode as a String
    if (schema.annotations.exists(_.isInstanceOf[simpleEnum])) {
      Writes.StringWrites.contramap[Z](caseMap(schema, config))
    } else {
      new Writes[Z] {

        val discriminatorName    =
          if (schema.noDiscriminator || (config.noDiscriminator && schema.discriminatorName.isEmpty)) None
          else schema.discriminatorName.orElse(config.discriminatorName)
        val cases                = schema.nonTransientCases.toArray
        val names                = cases.map { case_ => format(case_.caseName, config) }
        val writers              = cases.map { case_ =>
          val discriminatorTuple = discriminatorName.map(_ -> format(case_.caseName, config))
          writesSchema(case_.schema.asInstanceOf[Schema[Any]], config, discriminatorTuple)
        }
        val doJsonObjectWrapping = discriminatorName.isEmpty && !schema.noDiscriminator

        def writes(value: Z): JsValue = {
          var i = 0
          while (i < cases.size) {
            val case_ = cases(i)
            if (case_.isCase(value)) {
              val result = writers(i).writes(case_.deconstruct(value))
              return {
                if (doJsonObjectWrapping) Json.obj(names(i) -> result)
                else result
              }
            }
            i += 1
          }
          EmptyJsonObj // for transient cases
        }
      }
    }
  }

  def readsEnum[Z](parentSchema: Schema.Enum[Z], config: PlayJsonCodec.Configuration): Reads[Z] = {

    val caseNameAliases: mutable.HashMap[String, Schema.Case[Z, Any]] =
      Formats.caseNameAliases(parentSchema, config)

    if (parentSchema.cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) { // if all cases are CaseClass0, decode as String
      new Reads[Z] {

        val cases = new util.HashMap[String, Z](caseNameAliases.size << 1)
        caseNameAliases.foreach { case (name, case_) =>
          cases.put(format(name, config), case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct())
        }

        def reads(json: JsValue): JsResult[Z] = {
          Reads.StringReads.reads(json).flatMap { str =>
            if (str.exists(_.isWhitespace)) JsError(s"error.expected.validenumstring")
            else {
              val result = cases.get(str)
              if (result == null) JsError(s"error.unrecognized.subtype.$str")
              else JsSuccess(result)
            }
          }
        }
      }
    } else if (parentSchema.annotations.exists(_.isInstanceOf[noDiscriminator])) {
      new Reads[Z] {

        val readers = parentSchema.cases.toArray.map(c => readsSchema(c.schema, config))

        def reads(json: JsValue): JsResult[Z] = {
          val it = readers.iterator
          while (it.hasNext) {
            it.next().reads(json) match {
              case JsError(_)           =>
              case JsSuccess(result, _) =>
                return JsSuccess(result.asInstanceOf[Z])
            }
          }
          JsError("error.doesn-t.match.any.subtype")
        }
      }
    } else {
      val discriminator = parentSchema.discriminatorName.orElse(config.discriminatorName)
      discriminator match {
        case None       =>
          new Reads[Z] {

            val cases = new util.HashMap[String, Reads[Any]](caseNameAliases.size << 1)
            caseNameAliases.foreach { case (name, case_) =>
              cases.put(format(name, config), readsSchema(case_.schema, config, discriminator))
            }

            def reads(json: JsValue): JsResult[Z] = json match {
              case JsObject(underlying) =>
                underlying.toList match {
                  case Nil                   => JsError("error.missing.subtype")
                  case (field, value) :: Nil =>
                    if (field.exists(_.isWhitespace))
                      return JsError(JsPath \ field, s"error.expected.validenumstring")
                    val reads = cases.get(field)
                    if (reads == null) JsError(JsPath \ field, s"error.unrecognized.subtype")
                    else reads.reads(value).map(_.asInstanceOf[Z])
                  case _                     => JsError("error.too.many.fields")
                }
              case _                    => JsError(JsPath, JsonValidationError("error.expected.jsobject"))
            }
          }
        case Some(name) =>
          new Reads[Z] {

            val cases = new util.HashMap[String, Reads[Any]](caseNameAliases.size << 1)
            caseNameAliases.foreach { case (name, case_) =>
              cases.put(format(name, config), readsSchema(case_.schema, config, discriminator))
            }

            def reads(json: JsValue): JsResult[Z] = json match {
              case JsObject(underlying) =>
                underlying.get(name) match {
                  case None                => JsError(JsPath \ name, "error.path.missing")
                  case Some(JsString(cse)) =>
                    if (cse.exists(_.isWhitespace))
                      return JsError(JsPath \ name, s"error.expected.validenumstring")
                    val reads = cases.get(cse)
                    if (reads == null) JsError(JsPath \ name, s"error.unrecognized.subtype.$cse")
                    else reads.reads(json).map(_.asInstanceOf[Z])
                  case Some(_)             => JsError(JsPath \ name, "error.expected.enumstring")
                }
              case _                    => JsError(JsPath, JsonValidationError("error.expected.jsobject"))
            }
          }
      }
    }
  }

  def writesFallback[A, B](wl: Writes[A], wr: Writes[B]): Writes[Fallback[A, B]] = new Writes[Fallback[A, B]] {
    def writes(fallback: Fallback[A, B]): JsValue = fallback match {
      case Fallback.Left(a)    => wl.writes(a)
      case Fallback.Right(b)   => wr.writes(b)
      case Fallback.Both(a, b) => Writes.Tuple2W(wl, wr).writes((a, b))
    }
  }

  def readsFallback[A, B](schema: Schema.Fallback[A, B], config: PlayJsonCodec.Configuration): Reads[Fallback[A, B]] =
    new Reads[Fallback[A, B]] {

      val wl = readsSchema(schema.left, config)
      val wr = readsSchema(schema.right, config)

      def reads(json: JsValue): JsResult[Fallback[A, B]] = json match {
        case JsArray(arr) =>
          arr.toList match {
            case Nil                  => JsError(JsPath \ 0, "error.path.missing")
            case left :: right :: Nil =>
              wl.reads(left) match {
                case JsError(_)          => wr.reads(right).map(Fallback.Right(_))
                case JsSuccess(first, _) =>
                  if (!schema.fullDecode) JsSuccess(Fallback.Left(first))
                  else
                    wr.reads(right) match {
                      case JsError(_)           => JsSuccess(Fallback.Left(first))
                      case JsSuccess(second, _) => JsSuccess(Fallback.Both(first, second))
                    }
              }
            case xs                   =>
              val errors = new mutable.ListBuffer[(JsPath, Seq[JsonValidationError])]
              val i      = 2
              while (i < xs.size) {
                errors += JsPath \ i -> Seq(JsonValidationError("error.path.extra"))
              }
              JsError(errors.result())
          }
        case _            =>
          wl.reads(json).map(Fallback.Left(_)) match {
            case result: JsSuccess[_] => result
            case JsError(leftErrors)  =>
              wr.reads(json).map(Fallback.Right(_)) match {
                case result: JsSuccess[_] => result
                case JsError(rightErrors) => JsError(leftErrors ++ rightErrors)
              }
          }
      }
    }

  private def writesRecord[Z](
    schema: Schema.GenericRecord,
    config: PlayJsonCodec.Configuration,
    discriminatorTuple: DiscriminatorTuple,
  ): Writes[ListMap[String, _]] = {

    val nonTransientFields = schema.nonTransientFields.toArray
    val fieldNames         = nonTransientFields.map { field =>
      if (config.fieldNameFormat == NameFormat.Identity) field.fieldName
      else if (field.fieldName == field.name) config.fieldNameFormat(field.fieldName)
      else field.fieldName
    }
    val discriminator      = discriminatorTuple.map { case (tag, name) =>
      tag -> Writes.StringWrites.writes(name)
    }

    if (nonTransientFields.isEmpty) {
      new OWrites[ListMap[String, _]] {

        val result = discriminator match {
          case None        => EmptyJsonObj
          case Some(tuple) => JsObject(Map(tuple))
        }

        def writes(value: ListMap[String, _]): JsObject = result
      }
    } else {
      new OWrites[ListMap[String, _]] {

        val writers =
          nonTransientFields.map(field => writesSchema(field.schema.asInstanceOf[Schema[Any]], config))

        def writes(value: ListMap[String, _]): JsObject = {
          val builder = ListMap.newBuilder[String, JsValue]
          discriminator.foreach { case tuple =>
            builder += tuple
          }
          var i       = 0
          while (i < nonTransientFields.length) {
            val field      = nonTransientFields(i)
            val fieldName  = fieldNames(i)
            val fieldValue = value(field.fieldName)
            if (!isEmptyOptionalValue(field, fieldValue, config))
              builder += fieldName -> writers(i).writes(fieldValue)
            i += 1
          }
          JsObject(builder.result())
        }
      }
    }
  }

  private def isEmptyOptionalValue(schema: Schema.Field[_, _], value: Any, config: PlayJsonCodec.Configuration) = {
    (!config.explicitEmptyCollections.encoding || schema.optional) &&
    (value match {
      case None            => true
      case it: Iterable[_] => it.isEmpty
      case _               => false
    })
  }

  def readsRecord[Z](
    schema: Schema.GenericRecord,
    config: PlayJsonCodec.Configuration,
    discriminator: Option[String],
  ): Reads[ListMap[String, Any]] = new Reads[ListMap[String, Any]] {

    val fields           = schema.fields.toArray
    val fieldNames       = new Array[String](fields.length)
    val fieldWithReaders = new util.HashMap[String, (String, Reads[Any])](schema.fields.size << 1)

    var i = 0
    schema.fields.foreach { field =>
      val name  =
        if (config.fieldNameFormat == NameFormat.Identity) field.fieldName
        else if (field.fieldName == field.name) config.fieldNameFormat(field.fieldName)
        else field.fieldName
      fieldNames(i) = name
      val reads = readsSchema(field.schema, config).asInstanceOf[Reads[Any]]
      (field.nameAndAliases - field.fieldName + name).foreach { alias =>
        fieldWithReaders.put(alias, (name, reads))
      }
      i += 1
    }

    val explicitEmptyCollections = config.explicitEmptyCollections.decoding
    val explicitNulls            = config.explicitNullValues.decoding
    val rejectExtraFields        = schema.rejectExtraFields || config.rejectExtraFields

    def reads(json: JsValue): JsResult[ListMap[String, Any]] = json match {
      case JsObject(underlying) =>
        val map    = new util.HashMap[String, Any](fields.length << 1)
        val errors = new mutable.ListBuffer[(JsPath, scala.collection.Seq[JsonValidationError])]

        underlying.foreach { case (key, value) =>
          val fieldWithReads = fieldWithReaders.get(key)
          if (fieldWithReads != null) {
            val (field, reads) = fieldWithReads
            reads.reads(value) match {
              case error: JsError      => errors ++= error.repath(JsPath \ field).asInstanceOf[JsError].errors
              case JsSuccess(value, _) =>
                val prev = map.put(field, value)
                if (prev != null)
                  errors += JsPath \ field -> Seq(JsonValidationError("error.path.result.multiple"))
            }
          } else {
            if (rejectExtraFields && !discriminator.contains(key))
              errors += JsPath \ key -> Seq(JsonValidationError("error.path.extra"))
          }
        }

        var i = 0
        while (i < fields.length) {
          val field = fields(i)
          val name  = fieldNames(i)
          if (map.get(name) == null) {
            map.put( // mitigation of a linking error for `map.computeIfAbsent` in Scala.js
              name, {
                if ((field.optional || field.transient) && field.defaultValue.isDefined) {
                  field.defaultValue.get
                } else {
                  var schema = field.schema
                  schema match {
                    case l: Schema.Lazy[_] => schema = l.schema
                    case _                 =>
                  }
                  schema match {
                    case collection: Schema.Collection[_, _] if !explicitEmptyCollections => collection.empty
                    case _: Schema.Optional[_] if !explicitNulls                          => None
                    case _                                                                =>
                      errors += JsPath \ name -> Seq(JsonValidationError("error.path.missing"))
                      None
                  }
                }
              },
            )
          }
          i += 1
        }
        if (errors.nonEmpty)
          JsError(errors.result().groupBy(_._1).iterator.map { case (k, v) => k -> v.flatMap(_._2) }.toList)
        else {
          val builder =
            ListMap.newBuilder[String, Any] ++=
              ({ // to avoid O(n) insert operations
                import scala.collection.JavaConverters.mapAsScalaMapConverter // use deprecated class for Scala 2.12 compatibility
                map.asScala
              }: @scala.annotation.nowarn)

          JsSuccess(builder.result())
        }

      case _ => JsError(JsPath, JsonValidationError("error.expected.jsobject"))
    }
  }

  // scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  def writesCaseClass[A](schema: Schema.Record[A], config: PlayJsonCodec.Configuration, discriminatorTuple: DiscriminatorTuple): Writes[A] = new Writes[A] {

    val nonTransientFields = schema.nonTransientFields.map(_.asInstanceOf[Schema.Field[A, Any]]).toArray
    val fieldWriters       = nonTransientFields.map { s => writesSchema(s.schema, config) }
    val fieldNames         = nonTransientFields.map { field =>
      if (config.fieldNameFormat == NameFormat.Identity) field.fieldName
      else if (field.fieldName == field.name) config.fieldNameFormat(field.fieldName)
      else field.fieldName
    }
    val discriminator      = discriminatorTuple.map { case (tag, name) =>
      tag -> Writes.StringWrites.writes(name)
    }

    def writes(a: A): JsValue = {
      val builder = ListMap.newBuilder[String, JsValue]
      discriminator.foreach { case tuple =>
        builder += tuple
      }
      var i       = 0
      while (i < nonTransientFields.length) {
        val schema = nonTransientFields(i)
        val writes = fieldWriters(i)
        val value  = schema.get(a)
        val json   = writes.writes(value)
        if (!isEmptyOptionalValue(schema, value, config) && (json != JsNull || config.explicitNullValues.encoding))
          builder += fieldNames(i) -> json
        i += 1
      }
      JsObject(builder.result())
    }
  }

  def readsCaseClass0[Z](schema: Schema.CaseClass0[Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = new Reads[Z] {

    val rejectExtraFields = schema.rejectExtraFields || config.rejectExtraFields

    def reads(json: JsValue): JsResult[Z] = json match {
      case JsObject(underlying) =>
        discriminator match {
          case None       =>
            if (underlying.nonEmpty && rejectExtraFields) {
              val errors = new mutable.ListBuffer[(JsPath, Seq[JsonValidationError])]
              underlying.foreach { case (key, _) =>
                errors += JsPath \ key -> Seq(JsonValidationError("error.path.extra"))
              }
              JsError(errors.result())
            } else JsSuccess(schema.defaultConstruct())
          case Some(name) =>
            underlying.toList match {
              case Nil                 => JsSuccess(schema.defaultConstruct())
              case (field, value) :: _ =>
                if (field != name && rejectExtraFields) JsError(JsPath \ field, "error.path.extra")
                else if (field == name && value != JsString(schema.id.name))
                  JsError(JsPath \ field, s"error.expected.enumstring.${schema.id.name}")
                else JsSuccess(schema.defaultConstruct())
            }
        }
      case _                    => JsError(JsPath, JsonValidationError("error.expected.jsobject"))
    }
  }

  def readsCaseClass1[A, Z](schema: Schema.CaseClass1[A, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.defaultConstruct(buffer(0).asInstanceOf[A])
    }
  }

  def readsCaseClass2[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])
    }
  }

  def readsCaseClass3[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])
    }
  }

  def readsCaseClass4[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])
    }
  }

  def readsCaseClass5[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])
    }
  }

  def readsCaseClass6[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])
    }
  }

  def readsCaseClass7[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])
    }
  }

  def readsCaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])
    }
  }

  def readsCaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])
    }
  }

  def readsCaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])
    }
  }

  def readsCaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])
    }
  }

  def readsCaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])
    }
  }

  def readsCaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12], buffer(12).asInstanceOf[A13])
    }
  }

  def readsCaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
      )
    }
  }

  def readsCaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
      )
    }
  }

  def readsCaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
        buffer(15).asInstanceOf[A16],
      )
    }
  }

  def readsCaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
        buffer(15).asInstanceOf[A16],
        buffer(16).asInstanceOf[A17],
      )
    }
  }

  def readsCaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
        buffer(15).asInstanceOf[A16],
        buffer(16).asInstanceOf[A17],
        buffer(17).asInstanceOf[A18],
      )
    }
  }

  def readsCaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
        buffer(15).asInstanceOf[A16],
        buffer(16).asInstanceOf[A17],
        buffer(17).asInstanceOf[A18],
        buffer(18).asInstanceOf[A19],
      )
    }
  }

  def readsCaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
        buffer(15).asInstanceOf[A16],
        buffer(16).asInstanceOf[A17],
        buffer(17).asInstanceOf[A18],
        buffer(18).asInstanceOf[A19],
        buffer(19).asInstanceOf[A20],
      )
    }
  }

  def readsCaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
        buffer(15).asInstanceOf[A16],
        buffer(16).asInstanceOf[A17],
        buffer(17).asInstanceOf[A18],
        buffer(18).asInstanceOf[A19],
        buffer(19).asInstanceOf[A20],
        buffer(20).asInstanceOf[A21],
      )
    }
  }

  def readsCaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Z] = {
    readsFields(schema, config, discriminator).map { (buffer: Array[Any]) =>
      schema.construct(
        buffer(0).asInstanceOf[A1],
        buffer(1).asInstanceOf[A2],
        buffer(2).asInstanceOf[A3],
        buffer(3).asInstanceOf[A4],
        buffer(4).asInstanceOf[A5],
        buffer(5).asInstanceOf[A6],
        buffer(6).asInstanceOf[A7],
        buffer(7).asInstanceOf[A8],
        buffer(8).asInstanceOf[A9],
        buffer(9).asInstanceOf[A10],
        buffer(10).asInstanceOf[A11],
        buffer(11).asInstanceOf[A12],
        buffer(12).asInstanceOf[A13],
        buffer(13).asInstanceOf[A14],
        buffer(14).asInstanceOf[A15],
        buffer(15).asInstanceOf[A16],
        buffer(16).asInstanceOf[A17],
        buffer(17).asInstanceOf[A18],
        buffer(18).asInstanceOf[A19],
        buffer(19).asInstanceOf[A20],
        buffer(20).asInstanceOf[A21],
        buffer(21).asInstanceOf[A22],
      )
    }
  }

  private def readsFields[Z](schema: Schema.Record[Z], config: PlayJsonCodec.Configuration, discriminator: Option[String]): Reads[Array[Any]] = new Reads[Array[Any]] {

    val len     = schema.fields.length
    val fields  = new Array[Schema.Field[Z, _]](len)
    val readers = new Array[Reads[_]](len)
    val names   = new Array[String](len)
    val aliases = new util.HashMap[String, Int](len << 1)

    var i = 0
    schema.fields.foreach { field =>
      fields(i) = field
      readers(i) = readsSchema(field.schema, config)
      val name =
        if (config.fieldNameFormat == NameFormat.Identity) field.fieldName
        else if (field.fieldName == field.name) config.fieldNameFormat(field.fieldName)
        else field.fieldName
      names(i) = name
      (field.nameAndAliases - field.fieldName + name).foreach { alias => aliases.put(alias, i) }
      i += 1
    }

    discriminator.foreach { name => aliases.put(name, len) }

    val explicitEmptyCollections = config.explicitEmptyCollections.decoding
    val explicitNulls            = config.explicitNullValues.decoding
    val rejectExtraFields        = schema.rejectExtraFields || config.rejectExtraFields

    def reads(json: JsValue): JsResult[Array[Any]] = json match {
      case JsObject(underlying) =>
        val buffer = Array.ofDim[Any](len)
        val errors = new mutable.ListBuffer[(JsPath, scala.collection.Seq[JsonValidationError])]

        underlying.foreach { case (key, value) =>
          aliases.getOrDefault(key, -1) match {
            case -1            =>
              if (rejectExtraFields) errors += JsPath \ key -> Seq(JsonValidationError("error.path.extra"))
            case i if i == len => // check discriminator?
            case i             =>
              val field = names(i)
              if (buffer(i) != null) errors += JsPath \ field -> Seq(JsonValidationError("error.path.result.multiple"))
              else
                readers(i).reads(value) match {
                  case error: JsError      => errors ++= error.repath(JsPath \ field).asInstanceOf[JsError].errors
                  case JsSuccess(value, _) => buffer(i) = value
                }
          }
        }
        var i = 0
        while (i < len) {
          if (buffer(i) == null) {
            val field = fields(i)
            if ((field.optional || field.transient) && field.defaultValue.isDefined) {
              buffer(i) = field.defaultValue.get
            } else {
              var schema = field.schema
              schema match {
                case l: Schema.Lazy[_] => schema = l.schema
                case _                 =>
              }
              schema match {
                case collection: Schema.Collection[_, _] if !explicitEmptyCollections => buffer(i) = collection.empty
                case _: Schema.Optional[_] if !explicitNulls                          => buffer(i) = None
                case _                                                                =>
                  errors += JsPath \ { names(i) } -> Seq(JsonValidationError("error.path.missing"))
              }
            }
          }
          i += 1
        }

        if (errors.nonEmpty)
          JsError(errors.result().groupBy(_._1).iterator.map { case (k, v) => k -> v.flatMap(_._2) }.toList)
        else JsSuccess(buffer)

      case _ => JsError(JsPath, JsonValidationError("error.expected.jsobject"))
    }
  }
}
