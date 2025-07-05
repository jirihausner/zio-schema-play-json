package zio.schema.codec.play.json.internal

import play.api.libs.json.{Json, Writes}
import zio.Chunk

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets

trait StringUtils {

  private[play] def stringify(str: String): String = Json.stringify(Writes.StringWrites.writes(str))

  private[play] def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
    val bytes = StandardCharsets.UTF_8.newEncoder().encode(CharBuffer.wrap(chars))
    Chunk.fromByteBuffer(bytes)
  }
}
