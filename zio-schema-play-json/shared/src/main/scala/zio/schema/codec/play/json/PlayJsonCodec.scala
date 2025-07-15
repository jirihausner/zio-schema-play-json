package zio.schema.codec.play.json

import play.api.libs.json._
import zio.schema.codec.play.json.internal.{Formats, JsonSplitter}
import zio.schema.codec.{BinaryCodec, DecodeError}
import zio.schema.{NameFormat, Schema}
import zio.stream.ZPipeline
import zio.{Cause, Chunk}

object PlayJsonCodec {

  /**
   * When disabled for encoding, matching fields will be omitted from the JSON.
   * When disabled for decoding, missing fields will be decoded as default
   * value.
   */
  final case class ExplicitConfig(encoding: Boolean = true, decoding: Boolean = false)

  /**
   * Configuration for the JSON codec. The configurations are overruled by the
   * annotations that configure the same behavior.
   *
   * @param explicitEmptyCollections
   *   whether to encode empty collections as `[]` or omit the field and decode
   *   the field when it is missing as an empty collection or fail
   * @param explicitNulls
   *   whether to encode empty Options as `null` or omit the field and decode
   *   the field when it is missing to None or fail
   * @param discriminatorSettings
   *   set up how to handle discriminators
   * @param fieldNameFormat
   *   format for the field names
   * @param treatStreamsAsArrays
   *   whether to treat streams as arrays when encoding/decoding
   * @param rejectExtraFields
   *   whether to reject extra fields during decoding
   */
  final case class Configuration(
    explicitEmptyCollections: ExplicitConfig = ExplicitConfig(),
    explicitNullValues: ExplicitConfig = ExplicitConfig(),
    discriminatorSettings: DiscriminatorSetting = DiscriminatorSetting.default,
    fieldNameFormat: NameFormat = NameFormat.Identity,
    treatStreamsAsArrays: Boolean = false,
    rejectExtraFields: Boolean = false,
  ) {
    def withEmptyCollectionsIgnored: Configuration =
      copy(explicitEmptyCollections = ExplicitConfig(encoding = false, decoding = false))

    def withNullValuesIgnored: Configuration =
      copy(explicitNullValues = ExplicitConfig(encoding = false, decoding = false))

    def withNoDiscriminator: Configuration = copy(discriminatorSettings = DiscriminatorSetting.NoDiscriminator)

    def withDiscriminator(format: NameFormat): Configuration =
      copy(discriminatorSettings = DiscriminatorSetting.ClassName(format))

    def withDiscriminator(name: String, format: NameFormat = NameFormat.Identity): Configuration =
      copy(discriminatorSettings = DiscriminatorSetting.Name(name, format))

    def withFieldFormat(format: NameFormat): Configuration = copy(fieldNameFormat = format)

    def withStreamsTreatedAsArrays: Configuration = copy(treatStreamsAsArrays = true)

    def withExtraFieldsSkipped: Configuration = copy(rejectExtraFields = false)

    def withExtraFieldsRejected: Configuration = copy(rejectExtraFields = true)

    val noDiscriminator: Boolean = discriminatorSettings match {
      case DiscriminatorSetting.NoDiscriminator => true
      case _                                    => false
    }

    val discriminatorName: Option[String] = discriminatorSettings match {
      case DiscriminatorSetting.Name(name, _) => Some(name)
      case _                                  => None
    }

    val discriminatorFormat: NameFormat = discriminatorSettings match {
      case DiscriminatorSetting.ClassName(format) => format
      case DiscriminatorSetting.Name(_, format)   => format
      case _                                      => NameFormat.Identity
    }
  }

  object Configuration {
    val default: Configuration = Configuration()
  }

  sealed trait DiscriminatorSetting

  object DiscriminatorSetting {
    val default: ClassName = ClassName(NameFormat.Identity)
    case class ClassName(format: NameFormat)                                extends DiscriminatorSetting
    case object NoDiscriminator                                             extends DiscriminatorSetting
    case class Name(name: String, format: NameFormat = NameFormat.Identity) extends DiscriminatorSetting
  }

  object implicits {

    @inline
    implicit def playJsonBinaryCodec[A](implicit
      reads: Reads[A],
      writes: Writes[A],
      config: Configuration,
    ): BinaryCodec[A] = PlayJsonCodec.playJsonBinaryCodec(config)

    @inline
    implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A], config: Configuration): BinaryCodec[A] =
      PlayJsonCodec.schemaBasedBinaryCodec(config)

    @inline
    implicit def schemaFormat[A](implicit schema: Schema[A], config: Configuration): Format[A] =
      PlayJsonCodec.schemaFormat(schema)
  }

  @inline
  def playJsonBinaryCodec[A](implicit writes: Writes[A], reads: Reads[A]): BinaryCodec[A] =
    playJsonBinaryCodec(Configuration.default)

  def playJsonBinaryCodec[A](config: Configuration)(implicit reads: Reads[A], writes: Writes[A]): BinaryCodec[A] =
    new BinaryCodec[A] {

      override def encode(value: A): Chunk[Byte] = Chunk.fromArray(Json.toBytes(writes.writes(value)))

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = {
        if (config.treatStreamsAsArrays) {
          val interspersed: ZPipeline[Any, Nothing, A, Byte] = ZPipeline
            .mapChunks[A, Chunk[Byte]](_.map(encode))
            .intersperse(JsonSplitter.jsonArraySeparator)
            .flattenChunks
          val prepended: ZPipeline[Any, Nothing, A, Byte]    =
            interspersed >>> ZPipeline.prepend(JsonSplitter.jsonArrayPrefix)
          prepended >>> ZPipeline.append(JsonSplitter.jsonArrayPostfix)
        } else {
          ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).intersperse(JsonSplitter.jsonNdSeparator).flattenChunks
        }
      }

      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = {
        try {
          reads.reads(Json.parse(whole.toArray)) match {
            case error: JsError      => throw JsResult.Exception(error)
            case JsSuccess(value, _) => Right(value)
          }
        } catch {
          case exception: Throwable => Left(DecodeError.ReadError(Cause.fail(exception), exception.getMessage))
        }
      }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = {
        ZPipeline.fromChannel {
          ZPipeline.utf8Decode.channel.mapError(cce => DecodeError.ReadError(Cause.fail(cce), cce.getMessage))
        } >>>
          (if (config.treatStreamsAsArrays) JsonSplitter.splitJsonArrayElements
           else JsonSplitter.splitOnJsonBoundary) >>>
          ZPipeline.mapEitherChunked { (json: String) =>
            try {
              reads.reads(Json.parse(json)) match {
                case error: JsError      => throw JsResult.Exception(error)
                case JsSuccess(value, _) => Right(value)
              }
            } catch {
              case exception: Throwable => Left(DecodeError.ReadError(Cause.fail(exception), exception.getMessage))
            }
          }
      }
    }

  @inline
  def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    schemaBasedBinaryCodec(Configuration.default)

  def schemaBasedBinaryCodec[A](config: Configuration)(implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {

      private lazy val r: Reads[A]  = schemaReads(schema)(config)
      private lazy val w: Writes[A] = schemaWrites(schema)(config)

      override def encode(value: A): Chunk[Byte] = Chunk.fromArray(Json.toBytes(w.writes(value)))

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = {
        if (config.treatStreamsAsArrays) {
          val interspersed: ZPipeline[Any, Nothing, A, Byte] = ZPipeline
            .mapChunks[A, Chunk[Byte]](_.map(encode))
            .intersperse(JsonSplitter.jsonArraySeparator)
            .flattenChunks
          val prepended: ZPipeline[Any, Nothing, A, Byte]    =
            interspersed >>> ZPipeline.prepend(JsonSplitter.jsonArrayPrefix)
          prepended >>> ZPipeline.append(JsonSplitter.jsonArrayPostfix)
        } else {
          ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).intersperse(JsonSplitter.jsonNdSeparator).flattenChunks
        }
      }

      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = {
        try {
          r.reads(Json.parse(whole.toArray)) match {
            case error: JsError      => throw JsResult.Exception(error)
            case JsSuccess(value, _) => Right(value)
          }
        } catch {
          case exception: Throwable => Left(DecodeError.ReadError(Cause.fail(exception), exception.getMessage))
        }
      }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = {
        ZPipeline.fromChannel {
          ZPipeline.utf8Decode.channel.mapError(cce => DecodeError.ReadError(Cause.fail(cce), cce.getMessage))
        } >>>
          (if (config.treatStreamsAsArrays) JsonSplitter.splitJsonArrayElements
           else JsonSplitter.splitOnJsonBoundary) >>>
          ZPipeline.mapEitherChunked { (json: String) =>
            try {
              r.reads(Json.parse(json)) match {
                case error: JsError      => throw JsResult.Exception(error)
                case JsSuccess(value, _) => Right(value)
              }
            } catch {
              case exception: Throwable => Left(DecodeError.ReadError(Cause.fail(exception), exception.getMessage))
            }
          }
      }
    }

  @inline
  def schemaReads[A](schema: Schema[A])(implicit config: Configuration = Configuration.default): Reads[A] =
    Formats.readsSchema(schema, config)

  @inline
  def schemaWrites[A](schema: Schema[A])(implicit config: Configuration = Configuration.default): Writes[A] =
    Formats.writesSchema(schema, config)

  @inline
  def schemaFormat[A](schema: Schema[A])(implicit config: Configuration = Configuration.default): Format[A] =
    Format(schemaReads(schema), schemaWrites(schema))
}
