package zio.schema.codec.play.json.internal

import zio.schema._
import zio.schema.codec.play.json.internal.Data._
import zio.stream.ZStream
import zio.test._
import zio.{Console, ZIO}

/**
 * TODO: add missing writes-reads specs similarly to how zio-schema-json does it
 */
private[play] trait WritesReadsSpecs {

  type Config

  protected def DefaultConfig: Config

  protected def BinaryCodec[A]: (Schema[A], Config) => codec.BinaryCodec[A]

  final protected def assertWritesThenReadsFallback[A, B](
    schema: Schema.Fallback[A, B],
    value: Fallback[A, B],
  ): ZIO[Any, Nothing, TestResult] =
    ZStream
      .succeed(value)
      .via(
        BinaryCodec[zio.schema.Fallback[A, B]](schema, DefaultConfig).streamEncoder,
      )
      .runCollect
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .via(BinaryCodec[zio.schema.Fallback[A, B]](schema, DefaultConfig).streamDecoder)
          .runCollect
      }
      .either
      .flatMap { result =>
        val expected = if (schema.fullDecode) value else value.simplify
        result.map(_.headOption.getOrElse(expected)) match {
          case Right(obtained) =>
            if (expected == obtained)
              ZIO.succeed(assertTrue(expected == obtained))
            else
              assertWritesThenReadsFallback(schema, obtained)
          case Left(_)         => ZIO.succeed(assertTrue(false))
        }
      }

  final protected def assertWritesThenReads[A](
    schema: Schema[A],
    value: A,
    debug: Boolean = false,
  ): ZIO[Any, Nothing, TestResult] =
    assertWritesThenReadsWithDifferentSchemas(schema, schema, value, (x: A, y: A) => x == y, debug)

  final protected def assertWritesThenReadsWithDifferentSchemas[A1, A2](
    encodingSchema: Schema[A1],
    decodingSchema: Schema[A2],
    value: A1,
    compare: (A1, A2) => Boolean,
    debug: Boolean = false,
    config: Config = DefaultConfig,
  ): ZIO[Any, Nothing, TestResult] =
    ZStream
      .succeed(value)
      .tap(value => Console.printLine(s"Input Value: $value").when(debug).ignore)
      .via(BinaryCodec[A1](encodingSchema, config).streamEncoder)
      .runCollect
      .tap(encoded => Console.printLine(s"Encoded: ${new String(encoded.toArray)}").when(debug).ignore)
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .via(BinaryCodec[A2](decodingSchema, config).streamDecoder)
          .runCollect
          .tapError { err =>
            Console.printLineError(s"Decoding failed for input ${new String(encoded.toArray)}\nError Message: $err")
          }
      }
      .map(_.toList)
      .head
      .tap(decoded => Console.printLine(s"Decoded: $decoded").when(debug).ignore)
      .either
      .map { result =>
        assertTrue(
          result.toOption match {
            case None         => false
            case Some(value2) => compare(value, value2)
          },
        )
      }

  protected final val writeReadsSuite: Spec[Any, Nothing] = suite("encoding then decoding")(
    suite("primitive")(
      test("unit") {
        assertWritesThenReads(Schema[Unit], ())
      },
      test("string") {
        check(Gen.string) { string => assertWritesThenReads(Schema[String], string) }
      },
      test("boolean") {
        check(Gen.boolean) { boolean => assertWritesThenReads(Schema[Boolean], boolean) }
      },
    ),
    suite("either")(
      test("of primitives") {
        assertWritesThenReads(Schema[Either[String, String]], Left("foo")) &&
        assertWritesThenReads(Schema[Either[String, String]], Right("foo"))
      },
      test("of tuples") {
        val schema = Schema.tuple2[Int, Boolean]
        assertWritesThenReads(Schema.either(schema, schema), Left((1, true))) &&
        assertWritesThenReads(Schema.either(schema, schema), Right((2, false)))
      },
      test("of map") {
        val schema = Schema.either[Map[String, Boolean], Map[String, Boolean]]
        assertWritesThenReads[scala.util.Either[Map[Any, Any], Map[Any, Any]]](
          schema.asInstanceOf[Schema[scala.util.Either[Map[Any, Any], Map[Any, Any]]]],
          Left(Map("foo" -> true)).asInstanceOf[scala.util.Either[Map[Any, Any], Map[Any, Any]]],
        ) &&
        assertWritesThenReads[scala.util.Either[Map[Any, Any], Map[Any, Any]]](
          schema.asInstanceOf[Schema[scala.util.Either[Map[Any, Any], Map[Any, Any]]]],
          Right(Map("bar" -> false)).asInstanceOf[scala.util.Either[Map[Any, Any], Map[Any, Any]]],
        )
      },
      test("of set") {
        val schema = Schema.either[Set[String], Set[Int]]
        assertWritesThenReads[scala.util.Either[Set[Any], Set[Any]]](
          schema.asInstanceOf[Schema[scala.util.Either[Set[Any], Set[Any]]]],
          Left(Set("foo", "bar")).asInstanceOf[scala.util.Either[Set[Any], Set[Any]]],
        ) &&
        assertWritesThenReads[scala.util.Either[Set[Any], Set[Any]]](
          schema.asInstanceOf[Schema[scala.util.Either[Set[Any], Set[Any]]]],
          Right(Set(1, 2)).asInstanceOf[scala.util.Either[Set[Any], Set[Any]]],
        )
      },
    ),
    suite("record")(
      test("ADT with generic records and discriminator field") {
        assertWritesThenReads(
          OneOf4.schema,
          RecordExampleWithDiscriminator(f1 = Some("test")),
        )
      },
    ),
    suite("enumeration")(
      suite("of case classes and case objects with more than 64 cases")(
        test("without annotation")(
          assertWritesThenReads(Schema[BigEnum2], BigEnum2.Case69),
        ),
        test("with caseName")(
          assertWritesThenReads(Schema[BigEnum2], BigEnum2.Case00(123.toByte)),
        ),
        test("with caseAliases")(
          assertWritesThenReads(Schema[BigEnum2], BigEnum2.Case00(123.toByte)),
        ),
      ),
      suite("of case classes and case objects with more than 64 cases and discriminator field")(
        test("without annotation")(
          assertWritesThenReads(Schema[BigEnum3], BigEnum3.Case69),
        ),
        test("with caseName")(
          assertWritesThenReads(Schema[BigEnum3], BigEnum3.Case00(123.toByte)),
        ),
        test("with caseAliases")(
          assertWritesThenReads(Schema[BigEnum3], BigEnum3.Case00(123.toByte)),
        ),
      ),
    ),
  )
}
