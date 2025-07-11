package com.github.plokhotnyuk.jsoniter_scala.playjson

import scala.annotation.nowarn

private[playjson] object StringUtil {

  @nowarn
  @inline
  def toString(buf: Array[Byte], len: Int): String = new String(buf, 0, 1, len - 2)
}
