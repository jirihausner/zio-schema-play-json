package zio.schema.codec.play.json.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import play.api.libs.json.{JsValue, Writes}
import zio.durationInt
import zio.schema._
import zio.schema.codec.play.json._
import zio.schema.codec.play.json.internal._
import zio.test.TestAspect._
import zio.test._

object PlayJsonJsoniterCodecSpec extends ZIOSpecDefault with WritesSpecs with ReadsSpecs with WritesReadsSpecs {

  override type Config = PlayJsonCodec.Config

  override protected def DefaultConfig: PlayJsonCodec.Config = PlayJsonCodec.Config.default

  override protected def IgnoreEmptyCollectionsConfig: Config       =
    PlayJsonCodec.Config(ignoreEmptyCollections = true)
  override protected def KeepNullsAndEmptyColleciontsConfig: Config =
    PlayJsonCodec.Config(ignoreEmptyCollections = false, ignoreNullValues = false)
  override protected def StreamingConfig: PlayJsonCodec.Config      =
    PlayJsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)

  override protected def BinaryCodec[A]: (Schema[A], Config) => codec.BinaryCodec[A] =
    (schema: Schema[A], config: PlayJsonCodec.Config) => PlayJsonJsoniterCodec.schemaBasedBinaryCodec(config)(schema)

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
