package play.api.libs.json

import scala.annotation.nowarn

private[json] object StringUtil {

  @nowarn
  @inline
  def toString(buf: Array[Byte], len: Int): String = new String(buf, 0, 1, len - 2)
}
