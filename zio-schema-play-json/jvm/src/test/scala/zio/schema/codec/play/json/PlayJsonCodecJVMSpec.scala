package zio.schema.codec.play.json

import play.api.libs.json._
import zio.durationInt
import zio.schema.Schema
import zio.test.Assertion.{equalTo, isRight}
import zio.test.TestAspect.timeout
import zio.test._

import scala.util.control.NonFatal

object PlayJsonCodecJVMSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] =
    suite("PlayJsonCodec JVM Spec")(
      readingSuite,
    ) @@ TestAspect.jvmOnly @@ timeout(180.seconds)

  private val readingSuite = suite("reads")(
    suite("reads record with more than 22 fields")(
      test("successfully if missing fields in the json payload are populated with their default values") {
        val exampleSchema = zio.schema.DeriveSchema.gen[RecordExample]
        val string        = """{"f1": "test"}"""
        assertReadsJson(exampleSchema, RecordExample(Some("test")), string)
      },
      test("with failure if a field with no default value is missing in the json payload") {
        val exampleSchema = zio.schema.DeriveSchema.gen[RecordExample2]
        val string        = """{"f1": "test"}"""
        assertReadsJsonFailure(exampleSchema, string)
      },
    ),
  )

  private def assertReadsJson[A](schema: Schema[A], value: A, json: String) = {
    implicit val format: Format[A] = PlayJsonCodec.schemaFormat(schema)
    val either                     =
      try {
        format.reads(Json.parse(json)) match {
          case JsError(errors)     => Left(new Exception(errors.toString))
          case JsSuccess(value, _) => Right(value)
        }
      } catch { case ex if NonFatal(ex) => Left(ex) }
    zio.test.assert(either)(isRight(equalTo(value)))
  }

  private def assertReadsJsonFailure[A](schema: Schema[A], json: String) = {
    implicit val format: Format[A] = PlayJsonCodec.schemaFormat(schema)
    val either                     =
      try {
        format.reads(Json.parse(json)) match {
          case JsError(errors)     => Left(new Exception(errors.toString))
          case JsSuccess(value, _) => Right(value)
        }
      } catch { case ex if NonFatal(ex) => Left(ex) }
    zio.test.assertTrue(either.isLeft)
  }

  case class RecordExample(
    f1: Option[String],
    f2: Option[String] = None,
    f3: Option[String] = None,
    f4: Option[String] = None,
    f5: Option[String] = None,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    f23: Option[String] = None,
  )

  case class RecordExample2(
    f1: Option[String],
    f2: String,
    f3: Option[String] = None,
    f4: Option[String] = None,
    f5: Option[String] = None,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    f23: Option[String] = None,
  )
}
