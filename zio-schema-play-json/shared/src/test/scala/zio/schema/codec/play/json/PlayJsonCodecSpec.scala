package zio.schema.codec.play.json

import play.api.libs.json._
import zio._
import zio.schema._
import zio.schema.codec.DecodeError
import zio.schema.codec.play.json._
import zio.schema.codec.play.json.internal._
import zio.test.TestAspect._
import zio.test._

object PlayJsonCodecSpec extends ZIOSpecDefault with WritesSpecs with ReadsSpecs with WritesReadsSpecs {

  override type Config = PlayJsonCodec.Config

  override protected def DefaultConfig: PlayJsonCodec.Config = PlayJsonCodec.Config.default

  override protected def IgnoreEmptyCollectionsConfig: Config       =
    PlayJsonCodec.Config(ignoreEmptyCollections = true)
  override protected def KeepNullsAndEmptyColleciontsConfig: Config =
    PlayJsonCodec.Config(ignoreEmptyCollections = false, ignoreNullValues = false)
  override protected def StreamingConfig: Config                    =
    PlayJsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)

  override protected def BinaryCodec[A]: (Schema[A], Config) => codec.BinaryCodec[A] =
    (schema: Schema[A], config: PlayJsonCodec.Config) => PlayJsonCodec.schemaBasedBinaryCodec(config)(schema)

  val playJsonASTSuite: Spec[Any, DecodeError] = suite("Play JSON AST")(
    suite("play.api.lib.json.JsValue")(
      test("reads and writes null") {
        assertReads(schemaJsValue, """null""", JsNull) &&
        assertWrites(schemaJsValue, JsNull, """null""")
      },
      test("reads and writes any boolean") {
        check(Gen.boolean) { bool =>
          assertReads(schemaJsValue, bool.toString, JsBoolean(bool)) &&
          assertWrites(schemaJsValue, JsBoolean(bool), bool.toString)
        }
      },
      //   suite("strings")(
      //     test("encodes and decodes any string") {
      //       check(Gen.string) { string =>
      //         assertEncodes(schemaJson, Json.fromString(string), s""""$string"""") &&
      //         assertDecodes(schemaJson, s""""$string"""", Json.fromString(string))
      //       }
      //     },
      //     test("encodes and decodes any currency") {
      //       check(Gen.currency) { currency =>
      //         assertEncodes(schemaJson, Json.fromString(currency.toString), s""""${currency.toString}"""") &&
      //         assertDecodes(schemaJson, s""""${currency.toString}"""", Json.fromString(currency.toString))
      //       }
      //     } @@ TestAspect.jvmOnly,
      //   ),
      //   suite("numbers")(
      //     test("encodes and decodes integer") {
      //       check(Gen.int) { int =>
      //         assertEncodes(schemaJson, Json.fromInt(int), int.toString) &&
      //         assertDecodes(schemaJson, int.toString, Json.fromInt(int))
      //       }
      //     },
      //     test("decodes integer with zero fractional part") {
      //       check(Gen.int) { int =>
      //         assertDecodes(schemaJson, s"""$int.0""", Json.fromInt(int))
      //       }
      //     },
      //     test("decodes number with floating point") {
      //       assertDecodes(schemaJson, """1.1""", Json.fromDoubleOrNull(1.1))
      //     },
      //     test("decodes number with exponential notation") {
      //       assertDecodes(schemaJson, """2.99792458e8""", Json.fromBigDecimal(BigDecimal("2.99792458e8")))
      //     },
      //     test("decodes number in double quetes as string") {
      //       check(Gen.int) { int =>
      //         assertDecodes(schemaJson, s""""$int"""", Json.fromString(int.toString))
      //       }
      //     },
      //   ),
      //   suite("arrays")(
      //     test("encodes and decodes empty array") {
      //       assertEncodes(schemaJson, Json.arr(), """[]""") &&
      //       assertDecodes(schemaJson, """[]""", Json.arr())
      //     },
      //     test("encodes and decodes an array containing with null") {
      //       assertEncodes(
      //         schemaJson,
      //         Json.arr(Json.Null),
      //         """[null]""",
      //         CirceCodec.Config(ignoreEmptyCollections = false, ignoreNullValues = false),
      //       ) &&
      //       assertDecodes(
      //         schemaJson,
      //         """[null]""",
      //         Json.arr(Json.Null),
      //         CirceCodec.Config(ignoreEmptyCollections = false, ignoreNullValues = false),
      //       )
      //     },
      //     test("encodes an array containing without null") {
      //       assertEncodes(schemaJson, Json.arr(Json.Null), """[]""")
      //     },
      //     test("encodes and decodes any array of booleans") {
      //       check(Gen.listOf(Gen.boolean)) { bools =>
      //         assertEncodes(schemaJson, Json.arr(bools.map(Json.fromBoolean): _*), bools.mkString("[", ",", "]")) &&
      //         assertDecodes(schemaJson, bools.mkString("[", ",", "]"), Json.arr(bools.map(Json.fromBoolean): _*))
      //       }
      //     },
      //     test("encodes and decodes any array of ints") {
      //       check(Gen.listOf(Gen.int)) { ints =>
      //         assertEncodes(schemaJson, Json.arr(ints.map(Json.fromInt): _*), ints.mkString("[", ",", "]")) &&
      //         assertDecodes(schemaJson, ints.mkString("[", ",", "]"), Json.arr(ints.map(Json.fromInt): _*))
      //       }
      //     },
      //     test("encodes and decodes any array of strings") {
      //       check(Gen.listOf(Gen.string)) { strings =>
      //         val json = strings.map(str => Encoder[String].apply(str).noSpaces).mkString("[", ",", "]")
      //         assertEncodes(schemaJson, Json.arr(strings.map(Json.fromString): _*), json) &&
      //         assertDecodes(schemaJson, json, Json.arr(strings.map(Json.fromString): _*))
      //       }
      //     },
      //     test("encodes and decodes mixed array of booleans, ints and strings") {
      //       check(Gen.boolean <*> Gen.int <*> Gen.string) { case (bool, int, string) =>
      //         assertEncodes(
      //           schemaJson,
      //           Json.arr(Json.fromBoolean(bool), Json.fromInt(int), Json.fromString(string)),
      //           s"""[$bool,$int,"$string"]""",
      //         ) &&
      //         assertDecodes(
      //           schemaJson,
      //           s"""[$bool,$int,"$string"]""",
      //           Json.arr(Json.fromBoolean(bool), Json.fromInt(int), Json.fromString(string)),
      //         )
      //       }
      //     },
      //     test("encodes and decodes an array containing empty object") {
      //       assertEncodes(schemaJson, Json.arr(Json.obj()), """[{}]""") &&
      //       assertDecodes(schemaJson, """[{}]""", Json.arr(Json.obj()))
      //     },
      //     test("encodes and decodes an array containing non-empty object") {
      //       check(Gen.string <*> Gen.string) { case (key, value) =>
      //         assertEncodes(
      //           schemaJson,
      //           Json.arr(Json.obj(key -> Json.fromString(value))),
      //           s"""[{"$key":"$value"}]""",
      //         ) &&
      //         assertDecodes(
      //           schemaJson,
      //           s"""[{"$key":"$value"}]""",
      //           Json.arr(Json.obj(key -> Json.fromString(value))),
      //         )
      //       }
      //     },
      //   ),
      //   suite("objects")(
      //     test("encodes and decodes empty object") {
      //       assertEncodes(schemaJson, Json.obj(), """{}""") &&
      //       assertDecodes(schemaJson, """{}""", Json.obj())
      //     },
      //     test("encodes and decodes non-empty object") {
      //       assertEncodes(
      //         schemaJson,
      //         Json.obj("foo" -> Json.fromString("bar")),
      //         """{"foo":"bar"}""",
      //       ) &&
      //       assertDecodes(
      //         schemaJson,
      //         """{"foo":"bar"}""",
      //         Json.obj("foo" -> Json.fromString("bar")),
      //       )
      //     },
      //     test("encodes non-empty object with nulls") {
      //       assertEncodes(
      //         schemaJson,
      //         Json.obj("foo" -> Json.fromString("bar"), "null" -> Json.Null),
      //         """{"foo":"bar","null":null}""",
      //         CirceCodec.Config(ignoreEmptyCollections = true, ignoreNullValues = false),
      //       )
      //     },
      //     test("encodes non-empty object without nulls") {
      //       assertEncodes(
      //         schemaJson,
      //         Json.obj("foo" -> Json.fromString("bar"), "null" -> Json.Null),
      //         """{"foo":"bar"}""",
      //       )
      //     },
      //     test("decodes non-empty object with nulls") {
      //       assertDecodes(
      //         schemaJson,
      //         """{"foo":"bar","null":null}""",
      //         Json.obj("foo" -> Json.fromString("bar"), "null" -> Json.Null),
      //       )
      //     },
      //   ),
      // ),
      // suite("io.circe.JsonObject")(
      //   test("encodes empty object") {
      //     assertEncodes(schemaJsonObject, JsonObject.empty, """{}""")
      //   },
      //   test("decodes empty object") {
      //     assertDecodes(schemaJsonObject, """{}""", JsonObject.empty)
      //   },
      //   test("encodes non-empty object with nulls") {
      //     assertEncodes(
      //       schemaJsonObject,
      //       JsonObject("foo" -> Json.fromString("bar"), "null" -> Json.Null),
      //       """{"foo":"bar","null":null}""",
      //       CirceCodec.Config(ignoreEmptyCollections = false, ignoreNullValues = false),
      //     )
      //   },
      //   test("encodes non-empty object without nulls") {
      //     assertEncodes(
      //       schemaJsonObject,
      //       JsonObject("foo" -> Json.fromString("bar"), "null" -> Json.Null),
      //       """{"foo":"bar"}""",
      //     )
      //   },
      //   test("decodes non-empty object with nulls") {
      //     assertDecodes(
      //       schemaJsonObject,
      //       """{"foo":"bar","null":null}""",
      //       JsonObject("foo" -> Json.fromString("bar"), "null" -> Json.Null),
      //     )
      //   },
      // ),
      // suite("io.circe.JsonNumber")(
      //   test("decodes integer") {
      //     check(Gen.int) { int =>
      //       assertDecodes(schemaJsonNumber, int.toString, JsonNumber.fromDecimalStringUnsafe(int.toString))
      //     }
      //   },
      //   test("decodes integer with zero fractional part") {
      //     check(Gen.int) { int =>
      //       assertDecodes(schemaJsonNumber, s"""$int.0""", JsonNumber.fromDecimalStringUnsafe(int.toString))
      //     }
      //   },
      //   test("decodes number with floating point") {
      //     assertDecodes(schemaJsonNumber, """1.1""", JsonNumber.fromDecimalStringUnsafe("1.1"))
      //   },
      //   test("decodes number with exponential notation") {
      //     assertDecodes(schemaJsonNumber, """2.99792458e8""", JsonNumber.fromDecimalStringUnsafe("2.99792458e8"))
      //   },
      //   test("fails decoding number as string") {
      //     check(Gen.int) { int =>
      //       assertDecodesToError(
      //         schemaJsonNumber,
      //         s""""$int"""",
      //         DecodingFailure(s"""Failed to parse "$int" as number""", Nil),
      //       )
      //     }
      //   },
    ),
  )

  def spec: Spec[TestEnvironment, Any] =
    suite("PlayJsonCodec specs")(
      writesSuite,
      readsSuite,
      writeReadsSuite,
      playJsonASTSuite,
    ) @@ timeout(180.seconds)
}
