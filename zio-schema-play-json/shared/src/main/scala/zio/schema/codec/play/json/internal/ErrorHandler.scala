package zio.schema.codec.play.json.internal

import play.api.libs.json._
import zio.schema.codec.DecodeError
import zio.{Cause, Chunk}

import scala.annotation.tailrec

private[json] object ErrorHandler {

  def handle(error: JsError): DecodeError = {
    if (error.errors.isEmpty) {
      val exception = JsResult.Exception(error)
      DecodeError.ReadError(Cause.fail(exception), exception.getMessage)
    } else {
      error.errors.tail.foldLeft(handleError(error.errors.head)) { case (acc, error) =>
        DecodeError.And(acc, handleError(error))
      }
    }
  }

  private def handleError(error: (JsPath, scala.collection.Seq[JsonValidationError])): DecodeError = {
    val path      = pathToChunk(error._1)
    val readError =
      (message: String) =>
        if (path.isEmpty) DecodeError.ReadError(Cause.empty, message)
        else DecodeError.ReadErrorWithPath(path, Cause.empty, message)
    val errors    = error._2

    if (errors.isEmpty) readError("error.validation.missing")
    else {
      errors.tail.foldLeft[DecodeError](readError(errors.head.message)) { case (acc, error) =>
        DecodeError.And(acc, readError(error.message))
      }
    }
  }

  private def pathToChunk(path: JsPath): Chunk[String] = {

    @tailrec
    def rec(acc: Chunk[String], prev: Option[String], in: List[PathNode]): Chunk[String] = in match {
      case RecursiveSearch(key) :: rest => rec(prev.fold(acc)(acc :+ _), Some(key), rest)
      case KeyPathNode(key) :: rest     => rec(prev.fold(acc)(acc :+ _), Some(key), rest)
      case IdxPathNode(idx) :: rest     => rec(acc, prev.map(_ + s"[$idx]").orElse(Some(s"[$idx]")), rest)
      case Nil                          =>
        prev match {
          case Some(c) => acc :+ c
          case None    => acc
        }
    }

    rec(Chunk.empty, None, path.path)
  }
}
