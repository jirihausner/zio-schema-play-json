package zio.schema.codec.play.json.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, readFromString, writeToArray}
import play.api.libs.json._
import zio.schema.Schema
import zio.schema.codec.play.json.PlayJsonCodec.Configuration
import zio.schema.codec.play.json.internal.JsonSplitter
import zio.schema.codec.play.json.jsoniter.internal.Formats
import zio.schema.codec.{BinaryCodec, DecodeError}
import zio.stream.ZPipeline
import zio.{Cause, Chunk}

object PlayJsonJsoniterCodec {

  private[play] val jsValueCodec: JsonValueCodec[JsValue] = JsValueJsonValueCodec()

  object implicits {

    @inline
    implicit def playJsonJsoniterBinaryCodec[A](implicit
      reads: Reads[A],
      writes: Writes[A],
      config: Configuration,
    ): BinaryCodec[A] = PlayJsonJsoniterCodec.playJsonJsoniterBinaryCodec(config)

    @inline
    implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A], config: Configuration): BinaryCodec[A] =
      PlayJsonJsoniterCodec.schemaBasedBinaryCodec(config)

    @inline
    implicit def schemaFormat[A](implicit schema: Schema[A], config: Configuration): Format[A] =
      PlayJsonJsoniterCodec.schemaFormat(schema)
  }

  @inline
  def playJsonJsoniterBinaryCodec[A](implicit writes: Writes[A], reads: Reads[A]): BinaryCodec[A] =
    playJsonJsoniterBinaryCodec(Configuration.default)

  def playJsonJsoniterBinaryCodec[A](
    config: Configuration,
  )(implicit reads: Reads[A], writes: Writes[A]): BinaryCodec[A] = new BinaryCodec[A] {

    override def encode(value: A): Chunk[Byte] = Chunk.fromArray(writeToArray(writes.writes(value))(jsValueCodec))

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
        reads.reads(readFromArray(whole.toArray)(jsValueCodec)) match {
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
            reads.reads(readFromString(json)(jsValueCodec)) match {
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

      private lazy val r: Reads[A]  = schemaReads(schema)
      private lazy val w: Writes[A] = schemaWrites(schema)(config)

      override def encode(value: A): Chunk[Byte] = Chunk.fromArray(writeToArray(w.writes(value))(jsValueCodec))

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
          r.reads(readFromArray(whole.toArray)(jsValueCodec)) match {
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
              r.reads(readFromString(json)(jsValueCodec)) match {
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
