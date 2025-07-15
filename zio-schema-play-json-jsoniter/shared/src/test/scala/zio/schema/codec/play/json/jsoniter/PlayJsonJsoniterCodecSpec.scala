package zio.schema.codec.play.json.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import play.api.libs.json.{JsValue, Writes}
import zio.durationInt
import zio.schema._
import zio.schema.codec.play.json.PlayJsonCodec.{Configuration, ExplicitConfig}
import zio.schema.codec.play.json._
import zio.schema.codec.play.json.internal._
import zio.test.TestAspect._
import zio.test._

object PlayJsonJsoniterCodecSpec extends ZIOSpecDefault with WritesSpecs with ReadsSpecs with WritesReadsSpecs {

  override protected def IgnoreEmptyCollectionsConfig: Configuration       =
    Configuration.default.withEmptyCollectionsIgnored.withNullValuesIgnored
  override protected def KeepNullsAndEmptyColleciontsConfig: Configuration =
    Configuration.default.copy(
      explicitEmptyCollections = ExplicitConfig(decoding = true),
      explicitNullValues = ExplicitConfig(decoding = true),
    )
  override protected def StreamingConfig: Configuration                    =
    Configuration.default.copy(treatStreamsAsArrays = true)

  override protected def BinaryCodec[A]: (Schema[A], Configuration) => codec.BinaryCodec[A] =
    (schema: Schema[A], config: Configuration) => PlayJsonJsoniterCodec.schemaBasedBinaryCodec(config)(schema)

  import zio.schema.codec.play.json.jsoniter.schemaJsValue

  /**
   * Workaround for inconsistency between play-json and jsoniter in handling
   * Unicode escaping (e.g. "\u001E" vs "\u001e").
   */
  override private[play] def stringify(str: String): String =
    writeToString(Writes.StringWrites.writes(str).asInstanceOf[JsValue])(PlayJsonJsoniterCodec.jsValueCodec)

  def spec: Spec[TestEnvironment, Any] =
    suite("PlayJsonJsoniterCodec specs")(
      writesSuite,
      readsSuite,
      writeReadsSuite,
      PlayJsonCodecSpec.playJsonASTSuite,
    ) @@ timeout(180.seconds)
}
