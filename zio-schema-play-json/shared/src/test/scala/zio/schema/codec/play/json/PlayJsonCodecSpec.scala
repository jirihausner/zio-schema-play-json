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

  def playJsonASTSuite(implicit schemaJsValue: Schema[JsValue]): Spec[Any, DecodeError] = suite("Play JSON AST")(
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
      suite("strings")(
        test("writes and reads any string") {
          check(Gen.string) { string =>
            assertWrites(schemaJsValue, JsString(string), s""""$string"""") &&
            assertReads(schemaJsValue, s""""$string"""", JsString(string))
          }
        },
        test("writes and reads any currency") {
          check(Gen.currency) { currency =>
            assertWrites(schemaJsValue, JsString(currency.toString), s""""${currency.toString}"""") &&
            assertReads(schemaJsValue, s""""${currency.toString}"""", JsString(currency.toString))
          }
        } @@ TestAspect.jvmOnly,
      ),
      suite("numbers")(
        test("writes and reads integer") {
          check(Gen.int) { int =>
            assertWrites(schemaJsValue, JsNumber(int), int.toString) &&
            assertReads(schemaJsValue, int.toString, JsNumber(int))
          }
        },
        test("reads integer with zero fractional part") {
          check(Gen.int) { int =>
            assertReads(schemaJsValue, s"""$int.0""", JsNumber(int))
          }
        },
        test("reads number with floating point") {
          assertReads(schemaJsValue, """1.1""", JsNumber(1.1))
        },
        test("reads number with exponential notation") {
          assertReads(schemaJsValue, """2.99792458e8""", JsNumber(BigDecimal("2.99792458e8")))
        },
      ),
      suite("arrays")(
        test("writes and reads empty array") {
          assertWrites(schemaJsValue, JsArray.empty, """[]""") &&
          assertReads(schemaJsValue, """[]""", JsArray.empty)
        },
        test("writes and reads an array containing with null") {
          assertWrites(
            schemaJsValue,
            JsArray(Seq(JsNull)),
            """[null]""",
            PlayJsonCodec.Config(ignoreEmptyCollections = false, ignoreNullValues = false),
          ) &&
          assertReads(
            schemaJsValue,
            """[null]""",
            JsArray(Seq(JsNull)),
            PlayJsonCodec.Config(ignoreEmptyCollections = false, ignoreNullValues = false),
          )
        },
        test("writes an array containing without null") {
          assertWrites(schemaJsValue, JsArray(Seq(JsNull)), """[]""")
        },
        test("writes and reads any array of booleans") {
          check(Gen.listOf(Gen.boolean)) { bools =>
            assertWrites(schemaJsValue, JsArray(bools.map(JsBoolean(_))), bools.mkString("[", ",", "]")) &&
            assertReads(schemaJsValue, bools.mkString("[", ",", "]"), JsArray(bools.map(JsBoolean(_))))
          }
        },
        test("writes and reads any array of ints") {
          check(Gen.listOf(Gen.int)) { ints =>
            assertWrites(schemaJsValue, JsArray(ints.map(JsNumber(_))), ints.mkString("[", ",", "]")) &&
            assertReads(schemaJsValue, ints.mkString("[", ",", "]"), JsArray(ints.map(JsNumber(_))))
          }
        },
        test("writes and reads any array of strings") {
          check(Gen.listOf(Gen.string)) { strings =>
            val json = strings.map(str => Writes.StringWrites.writes(str)).mkString("[", ",", "]")
            assertWrites(schemaJsValue, JsArray(strings.map(JsString(_))), json) &&
            assertReads(schemaJsValue, json, JsArray(strings.map(JsString(_))))
          }
        },
        test("writes and reads mixed array of booleans, ints and strings") {
          check(Gen.boolean <*> Gen.int <*> Gen.string) { case (bool, int, string) =>
            assertWrites(
              schemaJsValue,
              JsArray(Seq(JsBoolean(bool), JsNumber(int), JsString(string))),
              s"""[$bool,$int,"$string"]""",
            ) &&
            assertReads(
              schemaJsValue,
              s"""[$bool,$int,"$string"]""",
              JsArray(Seq(JsBoolean(bool), JsNumber(int), JsString(string))),
            )
          }
        },
        test("writes and reads an array containing empty object") {
          assertWrites(schemaJsValue, JsArray(Seq(JsObject.empty)), """[{}]""") &&
          assertReads(schemaJsValue, """[{}]""", JsArray(Seq(JsObject.empty)))
        },
        test("writes and reads an array containing non-empty object") {
          check(Gen.string <*> Gen.string) { case (key, value) =>
            assertWrites(
              schemaJsValue,
              JsArray(Seq(JsObject(Map(key -> JsString(value))))),
              s"""[{"$key":"$value"}]""",
            ) &&
            assertReads(
              schemaJsValue,
              s"""[{"$key":"$value"}]""",
              JsArray(Seq(JsObject(Map(key -> JsString(value))))),
            )
          }
        },
      ),
      suite("objects")(
        test("writes and reads empty object") {
          assertWrites(schemaJsValue, JsObject.empty, """{}""") &&
          assertReads(schemaJsValue, """{}""", JsObject.empty)
        },
        test("writes and reads non-empty object") {
          assertWrites(
            schemaJsValue,
            JsObject(Map("foo" -> JsString("bar"))),
            """{"foo":"bar"}""",
          ) &&
          assertReads(
            schemaJsValue,
            """{"foo":"bar"}""",
            JsObject(Map("foo" -> JsString("bar"))),
          )
        },
        test("writes non-empty object with nulls") {
          assertWrites(
            schemaJsValue,
            JsObject(Map("foo" -> JsString("bar"), "null" -> JsNull)),
            """{"foo":"bar","null":null}""",
            PlayJsonCodec.Config(ignoreEmptyCollections = true, ignoreNullValues = false),
          )
        },
        test("writes non-empty object without nulls") {
          assertWrites(
            schemaJsValue,
            JsObject(Map("foo" -> JsString("bar"), "null" -> JsNull)),
            """{"foo":"bar"}""",
          )
        },
        test("reads non-empty object with nulls") {
          assertReads(
            schemaJsValue,
            """{"foo":"bar","null":null}""",
            JsObject(Map("foo" -> JsString("bar"), "null" -> JsNull)),
          )
        },
      ),
    ),
  )

  def spec: Spec[TestEnvironment, Any] =
    suite("PlayJsonCodec specs")(
      writesSuite,
      readsSuite,
      writeReadsSuite,
      playJsonASTSuite,
    ) @@ timeout(180.seconds) @@ tag("core")
}
