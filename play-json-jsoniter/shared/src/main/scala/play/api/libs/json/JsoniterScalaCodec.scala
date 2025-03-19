package play.api.libs.json

import com.github.plokhotnyuk.jsoniter_scala.core._

import java.math.RoundingMode
import scala.util.control.NonFatal

object JsoniterScalaCodec {

  private[this] val intMin  = java.math.BigDecimal.valueOf(Int.MinValue)
  private[this] val intMax  = java.math.BigDecimal.valueOf(Int.MaxValue)
  private[this] val longMin = java.math.BigDecimal.valueOf(Long.MinValue)
  private[this] val longMax = java.math.BigDecimal.valueOf(Long.MaxValue)

  /**
   * Converts an ASCII byte array to a JSON string.
   *
   * @param buf
   *   the ASCII byte array
   * @param len
   *   the length of the byte array
   * @return
   *   a JSON string
   */
  @inline
  def asciiStringToJString(buf: Array[Byte], len: Int): JsString = JsString(StringUtil.toString(buf, len))

  /**
   * Extracts a `String` value from a JsValue.
   *
   * @param c
   *   the `JsValue` value
   * @return
   *   the `String` value, or null if the cursor does not point to a string
   */
  @inline
  def stringValue(json: JsValue): String = json match {
    case JsString(str) => str
    case _             => null
  }

  def byteFormat(fromString: String => Byte): Format[Byte] = new Format[Byte] {

    @inline
    final def writes(x: Byte): JsValue = JsNumber(BigDecimal.valueOf(x.toLong))

    final def reads(json: JsValue): JsResult[Byte] = json match {
      case JsNumber(value) =>
        val bd = intValueExact(value.underlying)
        if (bd ne null) {
          val l = bd.intValue
          val b = l.toByte
          if (b == l) return JsSuccess(b)
        }
        fail()
      case JsString(value) =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _               => fail()
    }

    @inline
    private[this] def fail(): JsResult[Byte] = JsError("error.expected.byte")
  }

  def shortFormat(fromString: String => Short): Format[Short] = new Format[Short] {

    @inline
    final def writes(x: Short): JsValue = JsNumber(BigDecimal.valueOf(x.toLong))

    final def reads(json: JsValue): JsResult[Short] = json match {
      case JsNumber(value) =>
        val bd = intValueExact(value.underlying)
        if (bd ne null) {
          val l = bd.intValue
          val s = l.toShort
          if (s == l) return JsSuccess(s)
        }
        fail()
      case JsString(value) =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _               => fail()
    }

    @inline
    private[this] def fail(): JsResult[Short] = JsError("error.expected.short")
  }

  def intFormat(fromString: String => Int): Format[Int] = new Format[Int] {

    @inline
    final def writes(x: Int): JsValue = JsNumber(BigDecimal.valueOf(x.toLong))

    final def reads(json: JsValue): JsResult[Int] = json match {
      case JsNumber(value) =>
        val bd = intValueExact(value.underlying)
        if (bd ne null) return JsSuccess(bd.intValue)
        fail()
      case JsString(value) =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _               => fail()
    }

    @inline
    private[this] def fail(): JsResult[Int] = JsError("error.expected.int")
  }

  def longFormat(fromString: String => Long): Format[Long] = new Format[Long] {

    @inline
    final def writes(x: Long): JsValue = JsNumber(BigDecimal.valueOf(x))

    final def reads(json: JsValue): JsResult[Long] = json match {
      case JsNumber(value) =>
        val bd = longValueExact(value.underlying)
        if (bd ne null) return JsSuccess(bd.longValue)
        fail()
      case JsString(value) =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _               => fail()
    }

    @inline
    private[this] def fail(): JsResult[Long] = JsError("error.expected.long")
  }

  def floatFormat(fromString: String => Float): Format[Float] = new Format[Float] {

    @inline
    final def writes(x: Float): JsValue = JsNumber(BigDecimal.decimal(x))

    final def reads(json: JsValue): JsResult[Float] = json match {
      case JsNumber(value) => JsSuccess(value.toFloat)
      case JsString(value) =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _               => fail()
    }

    @inline
    private[this] def fail(): JsResult[Float] = JsError("error.expected.float")
  }

  def doubleFormat(fromString: String => Double): Format[Double] = new Format[Double] {

    @inline
    final def writes(x: Double): JsValue = JsNumber(BigDecimal.decimal(x))

    final def reads(json: JsValue): JsResult[Double] = json match {
      case JsNumber(value) => JsSuccess(value.toDouble)
      case JsString(value) =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _               => fail()
    }

    @inline
    private[this] def fail(): JsResult[Double] = JsError("error.expected.double")
  }

  def bigIntFormat(fromString: String => BigInt): Format[BigInt] = new Format[BigInt] {

    @inline
    final def writes(x: BigInt): JsValue = {
      if (x.isValidLong) JsNumber(BigDecimal.valueOf(x.longValue))
      else JsNumber(new BigDecimal(new java.math.BigDecimal(x.bigInteger)))
    }

    final def reads(json: JsValue): JsResult[BigInt] = {
      try {
        json match {
          case JsNumber(value) =>
            JsSuccess {
              new BigInt({
                val bd = value.underlying
                if (bd.scale == 0) bd.unscaledValue
                else bd.toBigIntegerExact
              })
            }
          case JsString(value) => JsSuccess(fromString(value))
          case _               => fail()
        }
      } catch {
        case e if NonFatal(e) => fail()
      }
    }

    @inline
    private[this] def fail(): JsResult[BigInt] = JsError("error.expected.bigint")
  }

  def bigDecimalFormat(fromString: String => BigDecimal): Format[BigDecimal] = new Format[BigDecimal] {

    @inline
    final def writes(x: BigDecimal): JsValue = JsNumber(x)

    final def reads(json: JsValue): JsResult[BigDecimal] = json match {
      case JsNumber(value) => JsSuccess(new BigDecimal(value.underlying, JsonReader.bigDecimalMathContext))
      case JsString(value) =>
        try JsSuccess(fromString(value))
        catch {
          case e if NonFatal(e) => fail()
        }
      case _               => fail()
    }

    @inline
    private[this] def fail(): JsResult[BigDecimal] = JsError("error.expected.bigdecimal")
  }

  private[this] def intValueExact(bd: java.math.BigDecimal): java.math.BigDecimal = {
    if (bd.signum != 0) {
      var p   = bd.precision
      val s   = bd.scale
      if (p <= s || p - 10 > s) return null
      val bd0 = bd.setScale(0, RoundingMode.UNNECESSARY)
      p = bd0.precision
      if (p > 10 || p == 10 && (bd0.compareTo(intMin) < 0 || bd0.compareTo(intMax) > 0)) return null
      bd0
    } else java.math.BigDecimal.ZERO
  }

  private[this] def longValueExact(bd: java.math.BigDecimal): java.math.BigDecimal = {
    if (bd.signum != 0) {
      var p   = bd.precision
      val s   = bd.scale
      if (p <= s || p - 19 > s) return null
      val bd0 = bd.setScale(0, RoundingMode.UNNECESSARY)
      p = bd0.precision
      if (p > 19 || p == 19 && (bd0.compareTo(longMin) < 0 || bd0.compareTo(longMax) > 0)) return null
      bd0
    } else java.math.BigDecimal.ZERO
  }
}
