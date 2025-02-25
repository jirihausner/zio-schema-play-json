package zio.schema.codec.play.json.internal

import play.api.libs.json.{JsError, JsPath, JsResult, JsonValidationError}
import zio.prelude.NonEmptyMap
import zio.schema._
import zio.schema.annotation._
import zio.schema.codec.DecodeError
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.play.json.internal.Data._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._
import zio.{Cause, Chunk, Console, ZIO}

import scala.collection.immutable.ListMap

private[play] trait ReadsSpecs {

  type Config

  protected def DefaultConfig: Config
  protected def StreamingConfig: Config // should keep empty collections and treat streams as arrays

  protected def BinaryCodec[A]: (Schema[A], Config) => codec.BinaryCodec[A]

  final protected def assertReadsToError[A](
    schema: Schema[A],
    json: CharSequence,
    error: JsError,
    config: Config = DefaultConfig,
    debug: Boolean = false,
  ): ZIO[Any, Nothing, TestResult] = {
    val exception = JsResult.Exception(error)
    val stream    = ZStream
      .fromChunk(charSequenceToByteChunk(json))
      .via(BinaryCodec(schema, config).streamDecoder)
      .runHead
      .exit
      .tap { exit =>
        val expected = zio.Exit.Failure(Cause.fail(ReadError(Cause.fail(exception), exception.getMessage)))
        (Console.printLine(s"expected: $expected") *>
          Console.printLine(s"got:      $exit")).when(debug).ignore
      }
    assertZIO(stream)(fails(equalTo(ReadError(Cause.fail(exception), exception.getMessage))))
  }

  final protected def assertReadsToOneOfErrors[A](
    schema: Schema[A],
    json: CharSequence,
    errors: Seq[JsError],
    config: Config = DefaultConfig,
    debug: Boolean = false,
  ): ZIO[Any, Nothing, TestResult] = {
    val exceptions = errors.map { error =>
      val exception = JsResult.Exception(error)
      ReadError(Cause.fail(exception), exception.getMessage)
    }
    val stream     = ZStream
      .fromChunk(charSequenceToByteChunk(json))
      .via(BinaryCodec(schema, config).streamDecoder)
      .runHead
      .exit
      .tap { exit =>
        ZIO
          .when(debug) {
            Console.print("expected one of:") *>
              ZIO.foreach(exceptions)(exception => Console.printLine(zio.Exit.Failure(Cause.fail(exception)))) *>
              Console.printLine(s"got: $exit")
          }
          .ignore
      }
    assertZIO(stream)(fails(isOneOf(exceptions)))
  }

  final protected def assertReads[A](
    schema: Schema[A],
    json: CharSequence,
    value: A,
    config: Config = DefaultConfig,
    debug: Boolean = false,
  ): ZIO[Any, DecodeError, TestResult] = {
    val result = ZStream
      .fromChunk(charSequenceToByteChunk(json))
      .via(BinaryCodec(schema, config).streamDecoder)
      .runCollect
      .exit
      .tap { exit =>
        val expected = zio.Exit.Success(Chunk(value))
        (Console.printLine(s"expected: $expected") *>
          Console.printLine(s"got:      $exit")).when(debug).ignore
      }
    assertZIO(result)(succeeds(equalTo(Chunk(value))))
  }

  final protected def assertReadsMany[A](
    schema: Schema[A],
    json: CharSequence,
    values: Chunk[A],
    config: Config = DefaultConfig,
    debug: Boolean = false,
  ): ZIO[Any, DecodeError, TestResult] = {
    val result = ZStream
      .fromChunk(charSequenceToByteChunk(json))
      .via(BinaryCodec(schema, config).streamDecoder)
      .runCollect
      .exit
      .tap { exit =>
        val expected = zio.Exit.Success(values)
        (Console.printLine(s"expected: $expected") *>
          Console.printLine(s"got:      $exit")).when(debug).ignore
      }
    assertZIO(result)(succeeds(equalTo(values)))
  }

  import PaymentMethod.WireTransfer
  import Subscription.{OneTime, Recurring}

  protected final val readsSuite: Spec[Sized, DecodeError] = suite("reads")(
    suite("primitive")(
      test("Unit") {
        assertReads(Schema[Unit], "{}", ())
      },
      suite("String")(
        test("any unicode") {
          check(Gen.string) { string => assertReads(Schema[String], stringify(string), string) }
        },
        test("any ascii") {
          check(Gen.asciiString) { string => assertReads(Schema[String], stringify(string), string) }
        },
      ),
      test("Boolean") {
        check(Gen.boolean) { boolean =>
          assertReads(Schema.Primitive(StandardType.BoolType), boolean.toString, boolean)
        }
      },
      test("Byte") {
        check(Gen.byte) { byte =>
          assertReads(Schema.Primitive(StandardType.ByteType), byte.toString, byte)
        }
      },
      test("Short") {
        check(Gen.short) { short =>
          assertReads(Schema.Primitive(StandardType.ShortType), short.toString, short)
        }
      },
      test("Int") {
        check(Gen.int) { int =>
          assertReads(Schema.Primitive(StandardType.IntType), int.toString, int)
        }
      },
      test("Long") {
        check(Gen.long) { long =>
          assertReads(Schema.Primitive(StandardType.LongType), long.toString, long)
        }
      } @@ TestAspect.js(ignore), // FIXME: fix accuracy on ScalaJS platform
      test("Float") {
        check(Gen.float) { float =>
          assertReads(Schema.Primitive(StandardType.FloatType), float.toString, float)
        }
      },
      test("Double") {
        check(Gen.double) { double =>
          assertReads(Schema.Primitive(StandardType.DoubleType), double.toString, double)
        }
      },
      test("Binary") {
        check(Gen.chunkOf(Gen.byte)) { bytes =>
          assertReads(Schema.Primitive(StandardType.BinaryType), bytes.mkString("[", ",", "]"), bytes)
        }
      },
      test("Char") {
        check(Gen.asciiChar) { char =>
          assertReads(Schema.Primitive(StandardType.CharType), stringify(char.toString), char)
        }
      },
      test("BigInteger") {
        check(Gen.bigIntegerJava(BigInt(0), BigInt(Int.MaxValue))) { bi =>
          assertReads(Schema.Primitive(StandardType.BigIntegerType), bi.toString, bi)
        }
      },
      test("BigDecimal") {
        check(Gen.bigDecimalJava(BigDecimal(0), BigDecimal(Int.MaxValue))) { bd =>
          assertReads(Schema.Primitive(StandardType.BigDecimalType), bd.toString, bd)
        }
      },
      test("UUID") {
        check(Gen.uuid) { uuid =>
          assertReads(Schema.Primitive(StandardType.UUIDType), stringify(uuid.toString), uuid)
        }
      },
      test("DayOfWeek") {
        check(Gen.dayOfWeek) { dayOfWeek =>
          assertReads(Schema.Primitive(StandardType.DayOfWeekType), stringify(dayOfWeek.toString), dayOfWeek)
        }
      },
      test("Duration") {
        check(Gen.finiteDuration) { duration =>
          assertReads(Schema.Primitive(StandardType.DurationType), stringify(duration.toString), duration)
        }
      },
      test("Instant") {
        check(Gen.instant) { instant =>
          assertReads(Schema.Primitive(StandardType.InstantType), stringify(instant.toString), instant)
        }
      },
      test("LocalDate") {
        check(Gen.localDate) { localDate =>
          assertReads(Schema.Primitive(StandardType.LocalDateType), stringify(localDate.toString), localDate)
        }
      },
      test("LocalDateTime") {
        check(Gen.localDateTime) { localDateTime =>
          assertReads(
            Schema.Primitive(StandardType.LocalDateTimeType),
            stringify(localDateTime.toString),
            localDateTime,
          )
        }
      },
      test("LocalTime") {
        check(Gen.localTime) { localTime =>
          assertReads(
            Schema.Primitive(StandardType.LocalTimeType),
            stringify(localTime.toString),
            localTime,
          )
        }
      },
      test("Month") {
        check(Gen.month) { month =>
          assertReads(Schema.Primitive(StandardType.MonthType), stringify(month.toString), month)
        }
      },
      test("MonthDay") {
        check(Gen.monthDay) { monthDay =>
          assertReads(Schema.Primitive(StandardType.MonthDayType), stringify(monthDay.toString), monthDay)
        }
      },
      test("OffsetDateTime") {
        check(Gen.offsetDateTime) { offsetDateTime =>
          assertReads(
            Schema.Primitive(StandardType.OffsetDateTimeType),
            stringify(offsetDateTime.toString),
            offsetDateTime,
          )
        }
      },
      test("OffsetTime") {
        check(Gen.offsetTime) { offsetTime =>
          assertReads(Schema.Primitive(StandardType.OffsetTimeType), stringify(offsetTime.toString), offsetTime)
        }
      },
      test("Period") {
        check(Gen.period) { period =>
          assertReads(Schema.Primitive(StandardType.PeriodType), stringify(period.toString), period)
        }
      },
      test("Year") {
        check(Gen.year) { year =>
          assertReads(Schema.Primitive(StandardType.YearType), stringify(f"${year.getValue}%+d"), year)
        }
      },
      test("YearMonth") {
        check(Gen.yearMonth) { yearMonth =>
          assertReads(
            Schema.Primitive(StandardType.YearMonthType),
            if (yearMonth.getYear > 9999) stringify(s"+${yearMonth.toString}")
            else stringify(yearMonth.toString),
            yearMonth,
          )
        }
      },
      test("ZoneDateTime") {
        check(Gen.zonedDateTime) { zonedDateTime =>
          assertReads(
            Schema.Primitive(StandardType.ZonedDateTimeType),
            stringify(zonedDateTime.toString),
            zonedDateTime,
          )
        }
      },
      test("ZoneOffset") {
        check(Gen.zoneOffset) { zoneOffset =>
          assertReads(Schema.Primitive(StandardType.ZoneOffsetType), stringify(zoneOffset.toString), zoneOffset)
        }
      },
      test("ZoneId") {
        check(Gen.zoneId) { zoneId =>
          assertReads(Schema.Primitive(StandardType.ZoneIdType), stringify(zoneId.toString), zoneId)
        }
      },
      test("Currency") {
        check(Gen.currency) { currency =>
          assertReads(Schema[java.util.Currency], stringify(currency.toString), currency)
        }
      } @@ TestAspect.jvmOnly,
    ),
    suite("sequence")(
      test("of primitives") {
        check(Gen.chunkOf(Gen.string)) { chunk =>
          assertReads(
            Schema.chunk(Schema.Primitive(StandardType.StringType)),
            chunk.map(stringify).mkString("[", ",", "]"),
            chunk,
          )
        }
      },
      test("of complex values") {
        check(Gen.chunkOf(genValue)) { chunk =>
          assertReads(
            Schema.chunk(Value.schema),
            chunk.map { case Value(x, y) => s"""{"first":$x,"second":$y}""" }.mkString("[", ",", "]"),
            chunk,
          )
        }
      },
    ),
    suite("non-empty sequence")(
      test("of primitives") {
        check(genNonEmptyChunkOf(Gen.string)) { nonEmptyChunk =>
          assertReads(
            Schema.nonEmptyChunk(Schema.Primitive(StandardType.StringType)),
            nonEmptyChunk.map(stringify).mkString("[", ",", "]"),
            nonEmptyChunk,
          )
        }
      },
      test("of complex values") {
        check(genNonEmptyChunkOf(genValue)) { nonEmptyChunk =>
          assertReads(
            Schema.nonEmptyChunk(Value.schema),
            nonEmptyChunk.map { case Value(x, y) => s"""{"first":$x,"second":$y}""" }.mkString("[", ",", "]"),
            nonEmptyChunk,
          )
        }
      },
    ),
    suite("map")(
      test("of simple keys and values") {
        assertReads(
          Schema.map[Int, Value],
          """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}""",
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
        )
      },
      test("of simple keys and values where the key's schema is lazy") {
        assertReads(
          Schema.map[Int, Value](Schema.defer(Schema[Int]), Schema[Value]),
          """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}""",
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
        )
      },
      test("of complex keys and values") {
        assertReads(
          Schema.map[Key, Value],
          """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]""",
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
        )
      },
      test("of complex keys with transformation to primitive keys") {
        assertReads(
          Schema.map[KeyWrapper, ValueWrapper],
          """{"wrapped_key_1":{"value":"some_value"},"wrapped_key_2":{"value":"some_other_value"}}""",
          Map(
            KeyWrapper("wrapped_key_1") -> ValueWrapper(value = "some_value"),
            KeyWrapper("wrapped_key_2") -> ValueWrapper(value = "some_other_value"),
          ),
        )
      },
    ),
    suite("non-empty map")(
      test("of simple keys and values") {
        assertReads(
          Schema.nonEmptyMap[Int, Value],
          """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}""",
          NonEmptyMap(0 -> Value(0, true), 1 -> Value(1, false)),
        )
      },
      test("of simple keys and values where the key's schema is lazy") {
        assertReads(
          Schema.nonEmptyMap[Int, Value](Schema.defer(Schema[Int]), Schema[Value]),
          """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}""",
          NonEmptyMap(0 -> Value(0, true), 1 -> Value(1, false)),
        )
      },
      test("of complex keys and values") {
        assertReads(
          Schema.nonEmptyMap[Key, Value],
          """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]""",
          NonEmptyMap(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
        )
      },
      test("of complex keys with transformation to primitive keys") {
        assertReads(
          Schema.nonEmptyMap[KeyWrapper, ValueWrapper],
          """{"wrapped_key_1":{"value":"some_value"},"wrapped_key_2":{"value":"some_other_value"}}""",
          NonEmptyMap(
            KeyWrapper("wrapped_key_1") -> ValueWrapper(value = "some_value"),
            KeyWrapper("wrapped_key_2") -> ValueWrapper(value = "some_other_value"),
          ),
        )
      },
    ) @@ ignore, // FIXME: find better test, NonEmptyMap ordering is non-deterministic
    suite("set")(
      test("of primitives") {
        check(Gen.setOf(Gen.string)) { set =>
          assertReads(
            Schema.set(Schema.Primitive(StandardType.StringType)),
            set.map(stringify).mkString("[", ",", "]"),
            set,
          )
        }
      },
      test("of complex values") {
        check(Gen.setOf(genValue)) { set =>
          assertReads(
            Schema.set(Value.schema),
            set.map { case Value(x, y) => s"""{"first":$x,"second":$y}""" }.mkString("[", ",", "]"),
            set,
          )
        }
      },
    ) @@ ignore, // FIXME: find better test, Set ordering is non-deterministic
    suite("non-empty set")(
      test("of primitives") {
        check(genNonEmptySetOf(Gen.string)) { nonEmptySet =>
          assertReads(
            Schema.nonEmptySet(Schema.Primitive(StandardType.StringType)),
            nonEmptySet.map(stringify).mkString("[", ",", "]"),
            nonEmptySet,
          )
        }
      },
      test("of complex values") {
        check(genNonEmptySetOf(genValue)) { nonEmptySet =>
          assertReads(
            Schema.nonEmptySet(Value.schema),
            nonEmptySet.map { case Value(x, y) => s"""{"first":$x,"second":$y}""" }.mkString("[", ",", "]"),
            nonEmptySet,
            debug = true,
          )
        }
      },
    ) @@ ignore, // FIXME: find better test, NonEmptySet ordering is non-deterministic
    suite("transform")(
      test("of simple string to its size as int") {
        check(Gen.string) { string =>
          assertReads(
            Schema.Transform[String, Int, Unit](
              Schema[String],
              str => Right(str.size),
              _ => Left("undefined"),
              Chunk.empty[Any],
              (),
            ),
            stringify(string),
            string.size,
          )
        }
      },
      test("of failing as undefined") {
        check(Gen.int) { int =>
          assertReadsToError(
            Schema.Transform[Int, String, Unit](
              Schema[Int],
              _ => Left("undefined"),
              str => Right(str.size),
              Chunk.empty[Any],
              (),
            ),
            int.toString,
            JsError("undefined"),
          )
        }
      },
    ),
    suite("tuple")(
      test("of primitives") {
        check(Gen.string <*> Gen.int) { case (string, int) =>
          assertReads(
            Schema.Tuple2(
              Schema.Primitive(StandardType.StringType),
              Schema.Primitive(StandardType.IntType),
            ),
            s"""[${stringify(string)},$int]""",
            (string, int),
          )
        }
      },
      test("of complex values") {
        check(Gen.string <*> genValue) { case (string, value) =>
          assertReads(
            Schema.Tuple2(
              Schema.Primitive(StandardType.StringType),
              Value.schema,
            ),
            s"""[${stringify(string)},${s"""{"first":${value.first},"second":${value.second}}"""}]""",
            (string, value),
          )
        }
      },
    ),
    suite("optional")(
      test("of some primitive") {
        check(Gen.string) { string =>
          assertReads(Schema.Optional(Schema.Primitive(StandardType.StringType)), stringify(string), Some(string))
        }
      },
      test("of absent primitive") {
        assertReads(Schema.Optional(Schema.Primitive(StandardType.StringType)), "null", None)
      },
      test("of some complex value") {
        check(genValue) { value =>
          assertReads(
            Schema.Optional(Value.schema),
            s"""{"first":${value.first},"second":${value.second}}""",
            Some(value),
          )
        }
      },
      test("of absent complex value") {
        assertReads(Schema.Optional(Value.schema), "null", None)
      },
    ),
    suite("fail")(
      test("of any cause") {
        assertReadsToError(Schema.Fail[Exception]("failed"), "null", JsError("failed"))
      },
    ),
    suite("generic record")(
      test("with extra fields") {
        assertReads(
          recordSchema,
          """{"foo":"s","bar":1,"baz":2}""",
          ListMap[String, Any]("foo" -> "s", "bar" -> 1),
        )
      },
      test("with missing fields") {
        assertReads(
          RecordExample.schema,
          """{"$f1":"test"}""",
          RecordExample(f1 = Some("test"), f2 = None),
        )
      },
      test("aliased field") {
        assertReads(
          RecordExample.schema,
          """{"$f1":"test", "field2":"alias"}""",
          RecordExample(f1 = Some("test"), f2 = Some("alias")),
        )
      },
      test("reject extra fields") {
        assertReadsToError(
          RecordExample.schema.annotate(rejectExtraFields()),
          """{"$f1":"test", "extraField":"extra"}""",
          JsError(JsPath \ "extraField", "error.path.extra"),
        )
      },
      test("optional field with schema or annotated default value") {
        assertReads(
          RecordExampleWithOptField.schema,
          """{"$f1":"test"}""",
          RecordExampleWithOptField(f1 = Some("test"), f2 = None, f4 = "", f5 = "hello"),
        )
      },
    ),
    suite("either")(
      test("left") {
        check(Gen.int) { int =>
          assertReads(
            Schema.Either(Schema[Int], Schema[String]),
            s"""{"Left":$int}""",
            Left(int),
          )
        }
      },
      test("right") {
        check(Gen.string) { string =>
          assertReads(
            Schema.Either(Schema[Int], Schema[String]),
            s"""{"Right":${stringify(string)}}""",
            Right(string),
          )
        }
      },
    ),
    suite("fallback")(
      test("correctly fallbacks to left") {
        check(Gen.int) { int =>
          assertReads(
            Schema.Fallback(Schema[Int], Schema[String]),
            int.toString,
            Fallback.Left(int),
          )
        }
      },
      test("correctly fallbacks to right") {
        check(Gen.string) { string =>
          assertReads(
            Schema.Fallback(Schema[Int], Schema[String]),
            stringify(string),
            Fallback.Right(string),
          )
        }
      },
      test("correctly fallbacks to left with full decode") {
        check(Gen.int) { int =>
          assertReads(
            Schema.Fallback(Schema[Int], Schema[String], fullDecode = true),
            s"""[$int,$int]""",
            Fallback.Left(int),
          )
        }
      },
      test("correctly fallbacks to right with full decode") {
        check(Gen.string) { string =>
          assertReads(
            Schema.Fallback(Schema[Int], Schema[String], fullDecode = true),
            s"""[${stringify(string)},${stringify(string)}]""",
            Fallback.Right(string),
          )
        }
      },
      test("correctly fallbacks to both with full decode") {
        check(Gen.int <*> Gen.string) { case (int, string) =>
          assertReads(
            Schema.Fallback(Schema[Int], Schema[String], fullDecode = true),
            s"""[$int,${stringify(string)}]""",
            Fallback.Both(int, string),
          )
        }
      },
    ),
    suite("lazy")(
      test("of primitive") {
        check(Gen.string) { string =>
          assertReads(
            Schema.defer(Schema[String]),
            stringify(string),
            string,
          )
        }
      },
      test("of complex values") {
        check(genValue) { value =>
          assertReads(
            Schema.defer(Value.schema),
            s"""{"first":${value.first},"second":${value.second}}""",
            value,
          )
        }
      },
    ),
    suite("record")(
      test("of primitives") {
        assertReads(
          recordSchema,
          """{"foo":"s","bar":1}""",
          ListMap[String, Any]("foo" -> "s", "bar" -> 1),
        )
      },
      test("of records") {
        assertReads(
          nestedRecordSchema,
          """{"l1":"s","l2":{"foo":"s","bar":1}}""",
          ListMap[String, Any]("l1" -> "s", "l2" -> ListMap("foo" -> "s", "bar" -> 1)),
        )
      },
      test("case class") {
        check(searchRequestGen) { request =>
          assertReads(
            searchRequestSchema,
            s"""{"query":"${request.query}","size":${request.size},"page":${request.page}""" +
              request.nextPage
                .map(x => s""","nextPage":${stringify(x)}}""")
                .getOrElse("""}"""),
            request,
          )
        }
      },
      test("case object") {
        assertReads(schemaObject, "{}", Singleton)
      },
      test("record with option fields encoded as null") {
        assertReads(
          recordWithOptionSchema,
          """{"foo":"s","bar":null}""",
          ListMap[String, Any]("foo" -> Some("s"), "bar" -> None),
        )
      },
      test("case class with option fields omitted when empty") {
        assertReads(
          WithOptionFields.schema,
          """{"a":"s"}""",
          WithOptionFields(Some("s"), None),
        )
      },
      test("reject extra fields") {
        assertReadsToError(
          PersonWithRejectExtraFields.schema,
          """{"name":"test","age":10,"extraField":10}""",
          JsError(JsPath \ "extraField", "error.path.extra"),
        ) &>
          assertReadsToError(
            schemaObject.annotate(rejectExtraFields()),
            """{"extraField":10}""",
            JsError(JsPath \ "extraField", "error.path.extra"),
          )
      },
      test("transient field annotation") {
        assertReads(
          searchRequestWithTransientFieldSchema,
          """{"query":"foo","page":10,"size":20,"nextPage":"bar"}""",
          SearchRequestWithTransientField("foo", 10, 20, "bar"),
        )
      },
      test("transient field annotation with default value in class definition") {
        assertReads(
          searchRequestWithTransientFieldSchema,
          """{"query":"test","page":0,"size":10}""",
          SearchRequestWithTransientField("test", 0, 10),
        )
      },
      test("transient field annotation with default value implicitly available for the field type") {
        case class CaseClassWithTransientField(transient: String)
        assertReads(
          Schema.CaseClass1[String, CaseClassWithTransientField](
            id0 = TypeId.fromTypeName("SearchRequestWithTransientField"),
            field0 = Schema.Field(
              name0 = "transient",
              schema0 = Schema[String],
              get0 = _.transient,
              set0 = (x, transient) => x.copy(transient = transient),
              annotations0 = Chunk(new transientField()),
            ),
            defaultConstruct0 = new CaseClassWithTransientField(_),
          ),
          """{}""",
          CaseClassWithTransientField(Schema[String].defaultValue.toOption.get),
        )
      },
      test("fieldDefaultValue") {
        assertReads(
          fieldDefaultValueSearchRequestSchema,
          """{"query":"test","page":0,"size":10}""",
          FieldDefaultValueSearchRequest("test", 0, 10, "test"),
        )
      },
      test("backticked field name") {
        assertReads(
          BacktickedFieldName.schema,
          """{"x-api-key":"test"}""",
          BacktickedFieldName("test"),
        )
      },
      test("field name with alias - id") {
        assertReads(
          Order.schema,
          """{"id":1,"value":10,"description":"test"}""",
          Order(1, BigDecimal.valueOf(10), "test"),
        )
      },
      test("field name with alias - order_id") {
        assertReads(
          Order.schema,
          """{"id":1,"value":10,"description":"test"}""",
          Order(1, BigDecimal.valueOf(10), "test"),
        )
      },
      test("field name with alias - no alias") {
        assertReads(
          Order.schema,
          """{"orderId":1,"value":10,"description":"test"}""",
          Order(1, BigDecimal.valueOf(10), "test"),
        )
      },
      test("with option fields encoded as null") {
        assertReads(
          recordWithOptionSchema,
          """{"foo":"s","bar":null}""",
          ListMap[String, Any]("foo" -> Some("s"), "bar" -> None),
        )
      },
      test("with transient fields encoded as implicitly available schema default values") {
        assertReads(
          recordWithTransientSchema,
          """{}""",
          ListMap[String, Any]("foo" -> "", "bar" -> 0),
        )
      },
      test("case class with option fields encoded as null") {
        assertReads(
          WithOptionFields.schema,
          """{"a":"s","b":null}""",
          WithOptionFields(Some("s"), None),
        )
      },
      test("case class with int option field present (at end) from pretty printed json") {
        assertReads(
          WithOptionFields.schema,
          """
            |{
            |  "a": "s",
            |  "b": 1
            |}
            |""".stripMargin,
          WithOptionFields(Some("s"), Some(1)),
        )
      },
      test("case class with option fields omitted when empty") {
        assertReads(
          WithOptionFields.schema,
          """{"a":"s"}""",
          WithOptionFields(Some("s"), None),
        )
      },
      test("case class with option fields accepts empty json object as value") {
        assertReadsToError(
          WithOptionFields.schema,
          """{"a":"s", "b":{}}""",
          JsError(JsPath \ "b", "error.expected.jsnumber"),
        )
      },
      test("case class with complex option field rejects empty json object as value") {
        assertReadsToOneOfErrors(
          WithComplexOptionField.schema,
          """{"order":{}}""",
          Seq(
            JsError(
              scala.collection.Seq(
                JsPath \ "order" \ "value"       -> Seq(JsonValidationError("error.path.missing")),
                JsPath \ "order" \ "orderId"     -> Seq(JsonValidationError("error.path.missing")),
                JsPath \ "order" \ "description" -> Seq(JsonValidationError("error.path.missing")),
              ),
            ),
            JsError(
              scala.collection.Seq(
                JsPath \ "order" \ "orderId"     -> Seq(JsonValidationError("error.path.missing")),
                JsPath \ "order" \ "value"       -> Seq(JsonValidationError("error.path.missing")),
                JsPath \ "order" \ "description" -> Seq(JsonValidationError("error.path.missing")),
              ),
            ),
          ),
        )
      },
      test("case class with complex option field correctly reads") {
        assertReads(
          WithComplexOptionField.schema,
          """{"order":{"id":1,"value":10,"description":"test"}}""",
          WithComplexOptionField(Some(Order(1, BigDecimal.valueOf(10), "test"))),
        )
      },
      suite("case class with more than 64 fields")(
        test("reads required and optional fields") {
          assertReads(
            BigProduct.schema,
            """{"f00":true}""",
            BigProduct(f00 = true, f67 = None, f68 = Nil, f69 = Vector.empty),
          )
        },
        test("fails when missing required fields") {
          assertReadsToError(
            BigProduct.schema,
            """{}""",
            JsError(JsPath \ "f00", "error.path.missing"),
          )
        },
        test("rejects extra fields") {
          assertReadsToError(
            BigProduct.schema.annotate(rejectExtraFields()),
            """{"f00":true,"extraField":10}""",
            JsError(JsPath \ "extraField", "error.path.extra"),
          )
        },
        test("rejects duplicated fields") {
          assertReadsToError(
            BigProduct.schema,
            """{"f00":true,"f01":10,"f-01":8}""",
            JsError(JsPath \ "f01", "error.path.result.multiple"),
          )
        },
        test("reads field name with alias - id") {
          assertReads(
            BigProduct.schema,
            """{"f00":true,"f-01":123}""",
            BigProduct(f00 = true, f01 = Some(123.toByte), f67 = None, f68 = Nil, f69 = Vector.empty),
          )
        },
      ),
      test("recursive data structure")(
        assertReads(
          Schema[Recursive],
          """{"n":{"n":null}}""",
          Recursive(Some(Recursive(None))),
        ),
      ),
    ),
    suite("enumeration")(
      test("of primitives") {
        assertReads(
          enumSchema,
          """{"string":"foo"}""",
          "foo",
        )
      },
      test("ADT") {
        assertReads(
          Schema[Enumeration],
          """{"oneOf":{"StringValue":{"value":"foo"}}}""",
          Enumeration(StringValue("foo")),
        )
      },
      test("ADT with annotation") {
        assertReads(
          Schema[Enumeration2],
          """{"oneOf":{"_type":"StringValue2","value":"foo2"}}""",
          Enumeration2(StringValue2("foo2")),
        )
      },
      suite("with discriminator")(
        test("case name annotation") {
          assertReads(
            Subscription.schema,
            """{"type":"recurring","period":"monthly","amount":100}""",
            Recurring("monthly", 100),
          )
        },
        test("case name annotation with empty fields") {
          assertReads(
            Subscription.schema,
            """{"type":"unlimited"}""",
            Subscription.Unlimited(None),
          )
        },
        test("case name aliases - first alias") {
          assertReads(
            Subscription.schema,
            """{"type":"one_time","amount":1000}""",
            OneTime(1000),
          )
        },
        test("case name aliases - second alias") {
          assertReads(
            Subscription.schema,
            """{"type":"onetime","amount":1000}""",
            OneTime(1000),
          )
        },
        test("case name aliases - type in the middle") {
          assertReads(
            Subscription.schema,
            """{"period":"monthly","type":"recurring","amount":100}""",
            Recurring("monthly", 100),
          )
        },
        test("case name aliases - type in the last place") {
          assertReads(
            Subscription.schema,
            """{"amount":1000,"type":"onetime"}""",
            OneTime(1000),
          )
        },
        test("case name - illegal discriminator value") {
          assertReadsToError(
            Subscription.schema,
            """{"amount":1000,"type":123}""",
            JsError(JsPath \ "type", "error.expected.enumstring"),
          )
        },
        test("case name - missing discriminator field") {
          assertReadsToError(
            Subscription.schema,
            """{"amount":1000}""",
            JsError(JsPath \ "type", "error.path.missing"),
          )
        },
        test("case name - empty fields") {
          assertReads(
            Subscription.schema,
            """{"type":"unlimited"}""",
            Subscription.Unlimited(None),
          )
        },
        suite("of case classes and case objects with more than 64 cases")(
          test("with caseName") {
            assertReads(
              Schema[BigEnum3],
              """{"b":123,"type":"Case_00"}""",
              BigEnum3.Case00(123.toByte),
            ) &>
              assertReadsToError(
                Schema[BigEnum3],
                """{"type":"Case00"}""",
                JsError(JsPath \ "type", "error.unrecognized.subtype.Case00"), // TODO: validenumstring
              )
          },
          test("with caseAliases") {
            assertReads(
              Schema[BigEnum3],
              """{"type":"Case-00","b":123}""",
              BigEnum3.Case00(123.toByte),
            )
          },
          test("fails on missing discriminator field") {
            assertReadsToError(Schema[BigEnum3], """{"b":123}""", JsError(JsPath \ "type", "error.path.missing")) &>
              assertReadsToError(Schema[BigEnum3], """{}""", JsError(JsPath \ "type", "error.path.missing"))
          },
          test("fails on invalid case") {
            assertReadsToError(
              Schema[BigEnum3],
              """{"type":"CaseXX"}""",
              JsError(JsPath \ "type", "error.unrecognized.subtype.CaseXX"), // TODO: validenumstring
            )
          },
        ),
      ),
      suite("without discriminator")(
        test("case name annotation") {
          assertReads(
            PaymentMethod.schema,
            """{"wire_transfer":{"accountNumber":"foo","bankCode":"bar"}}""",
            WireTransfer("foo", "bar"),
          )
        },
        test("missing discriminator") {
          assertReadsToError(
            PaymentMethod.schema,
            "{}",
            JsError("error.missing.subtype"),
          )
        },
        test("illegal discriminator case") {
          assertReadsToError(
            PaymentMethod.schema,
            """{"cash":{}}""",
            JsError(JsPath \ "cash", "error.unrecognized.subtype"), // TODO: validenumstring
          )
        },
        suite("of case classes and case objects with more than 64 cases")(
          test("with caseName") {
            assertReads(
              Schema[BigEnum2],
              """{"Case_00":{"b":123}}""",
              BigEnum2.Case00(123.toByte),
            ) &>
              assertReadsToError(
                Schema[BigEnum2],
                """{"Case00":{}}""",
                JsError(JsPath \ "Case00", "error.unrecognized.subtype"), // TODO: validenumstring
              )
          },
          test("with caseAliases") {
            assertReads(
              Schema[BigEnum2],
              """{"Case-00":{"b":123}}""",
              BigEnum2.Case00(123.toByte),
            )
          },
          test("no discriminator key") {
            assertReadsToError(Schema[BigEnum2], "{}", JsError("error.missing.subtype"))
          },
          test("invalid case") {
            assertReadsToError(
              Schema[BigEnum2],
              """{"CaseXX":{}}""",
              JsError(JsPath \ "CaseXX", "error.unrecognized.subtype"), // TODO: validenumstring
            )
          },
        ),
      ),
      suite("with no discriminator")(
        test("reads first case") {
          assertReads(
            Prompt.schema,
            """{"value":"hello"}""",
            Prompt.Single("hello"),
          )
        },
        test("reads second case") {
          assertReads(
            Prompt.schema,
            """{"value":["hello","world"]}""",
            Prompt.Multiple(List("hello", "world")),
          )
        },
        test("fails on unrecognized case") {
          assertReadsToError(
            Prompt.schema,
            """{"value":5}""",
            JsError("error.doesn-t.match.any.subtype"),
          )
        },
      ),
      test("respects the case name annotation") {
        assertReads(
          Enum23Cases.schema,
          """{"NumberOne":{"value":"foo"}}""",
          Enum23Cases.Case1("foo"),
        )
      },
      test("respects case aliases") {
        assertReads(
          Enum23Cases.schema,
          """{"One":{"value":"foo"}}""",
          Enum23Cases.Case1("foo"),
        )
      },
    ),
    suite("dynamic direct mapping")(
      test("record") {
        assertReads(
          Schema.dynamicValue.annotate(directDynamicMapping()),
          """{"foo":"s","bar":1}""",
          DynamicValue.Record(
            TypeId.Structural,
            ListMap(
              "foo" -> DynamicValue.Primitive("s", StandardType.StringType),
              "bar" -> DynamicValue.Primitive(java.math.BigDecimal.valueOf(1L), StandardType.BigDecimalType),
            ),
          ),
        )
      },
    ),
    suite("empty collections fields")(
      test("map") {
        assertReads(
          Schema[ListAndMapAndOption],
          """{"list":[]}""",
          ListAndMapAndOption(Nil, Map.empty, None),
        )
      },
      test("list") {
        assertReads(
          Schema[ListAndMapAndOption],
          """{"map":{}}""",
          ListAndMapAndOption(Nil, Map.empty, None),
        )
      },
      test("set") {
        assertReads(Schema[SetWrapper], """{}""", SetWrapper(Set.empty))
      },
      test("vector") {
        assertReads(Schema[VectorWrapper], """{}""", VectorWrapper(Vector.empty))
      },
      test("chunck") {
        assertReads(Schema[ChunkWrapper], """{}""", ChunkWrapper(Chunk.empty))
      },
    ),
    suite("streams")(
      suite("of integers")(
        test("reads a stream with multiple integers separated by newlines") {
          assertReadsMany(Schema[Int], "1\n2\n3\n4\n5", Chunk.fromIterable(1 to 5))
        },
        test("reads a stream with multiple integers separated by spaces") {
          assertReadsMany(Schema[Int], "1 2 3 4 5", Chunk.fromIterable(1 to 5))
        },
        test("reads a stream with multiple integers separated by commas and other non JSON number characters") {
          assertReadsMany(Schema[Int], "1 2, 3;;; 4x5", Chunk.fromIterable(1 to 5), debug = true)
        } @@ ignore, // FIXME: fails but should work
        test("reads a stream with multiple integers encoded as an array") {
          assertReadsMany(Schema[Int], "[1,2,3,4,5]", Chunk.fromIterable(1 to 5), StreamingConfig, debug = true)
        } @@ ignore, // FIXME: fails but should work
        test("reads a stream with multiple integers encoded as an array with additional whitespace") {
          assertReadsMany(
            Schema[Int],
            """
              |   [1,
              |2,3,
              |4,   5]   """.stripMargin,
            Chunk.fromIterable(1 to 5),
            StreamingConfig,
            debug = true,
          )
        } @@ ignore, // FIXME: fails but should work
      ),
      suite("of booleans")(
        test("reads a stream with multiple booleans separated by newlines") {
          assertReadsMany(Schema[Boolean], "true\ntrue\nfalse", Chunk(true, true, false))
        },
        test("reads a stream with multiple booleans separated by spaces") {
          assertReadsMany(Schema[Boolean], "true true false", Chunk(true, true, false))
        },
        test("reads a stream with multiple booleans as an array") {
          assertReadsMany(
            Schema[Boolean],
            "[true, true, false]",
            Chunk(true, true, false),
            StreamingConfig,
          )
        },
        test(
          "reads a stream with multiple booleans separated by commas and other non JSON boolean characters and not separated at all",
        ) {
          assertReadsMany(
            Schema[Boolean],
            "true true, falsefalse",
            Chunk(true, true, false, false),
          )
        },
      ),
      suite("of strings")(
        test("reads a stream with multiple strings separated by newlines") {
          assertReadsMany(Schema[String], "\"a\"\n\"b\"\n\"c\"", Chunk("a", "b", "c"))
        },
        test("reads a stream with multiple strings as an array") {
          assertReadsMany(
            Schema[String],
            "[\"a\", \"b\",\n\"c\"]",
            Chunk("a", "b", "c"),
            StreamingConfig,
          )
        },
        test("reads a stream with multiple strings separated by spaces, commas and not separated at all") {
          assertReadsMany(Schema[String], """"a" "b","c""d"""", Chunk("a", "b", "c", "d"))
        },
      ),
      suite("stream of records")(
        test("reads a stream with multiple records separated by newlines") {
          assertReadsMany(
            Person.schema,
            """{"name":"Alice","age":1}
              |{"name":"Bob","age":2}
              |{"name":"Charlie","age":3}""".stripMargin,
            Chunk(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3),
            ),
          )
        },
        test("reads a stream with multiple records, not separated with internal newlines") {
          assertReadsMany(
            Person.schema,
            """{"name":"Alice","age":1}{"name":"Bob",
              |"age"
              |:2}{"name":"Charlie","age":3}""".stripMargin,
            Chunk(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3),
            ),
          )
        },
        test("reads a stream with multiple records formatted as an array") {
          assertReadsMany(
            Person.schema,
            """[{"name":"Alice","age":1},   {"name":"Bob","age":2},
              |{"name":"Charlie","age"
              |: 3}]""".stripMargin,
            Chunk(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3),
            ),
            StreamingConfig,
          )
        },
        test("reads a stream with no records") {
          assertReadsMany(Person.schema, "", Chunk.empty)
        },
        test("reads a stream with no records from an array") {
          assertReadsMany(Person.schema, "   [ ]  ", Chunk.empty, StreamingConfig)
        },
      ),
    ),
  )
}
