package zio.schema.codec.play.json.internal

import play.api.libs.json._

private[play] trait PlayJsonCompat {

  implicit class WritesOps[A](writes: Writes[A]) { self =>
    def contramap[B](f: B => A): Writes[B] = new Writes[B] {
      def writes(b: B): JsValue = self.writes.writes(f(b))
    }
  }

  implicit class ReadsOps[A](reads: Reads[A]) { self =>
    def flatMapResult[B](f: A => JsResult[B]): Reads[B] = new Reads[B] {
      def reads(json: JsValue): JsResult[B] = self.reads.reads(json).flatMap(f)
    }
  }

  trait KeyWrites[T] {
    def writeKey(key: T): String
  }

  object KeyWrites {
    def anyValKeyWrites[T <: AnyVal]: KeyWrites[T] = new KeyWrites[T] {
      def writeKey(key: T): String = key.toString
    }
  }

  trait KeyReads[T] { self =>
    def readKey(key: String): JsResult[T]
  }

  implicit class KeyReadsOps[A](reads: KeyReads[A]) { self =>
    def map[B](f: A => B): KeyReads[B] = new KeyReads[B] {
      def readKey(key: String): JsResult[B] = self.reads.readKey(key).map(f)
    }
  }

  object MapWrites {
    type Map[K, V] = scala.collection.Map[K, V]
  }

  implicit def keyMapWrites[K: KeyWrites, V: Writes, M[K, V] <: MapWrites.Map[K, V]]: OWrites[M[K, V]] =
    new OWrites[M[K, V]] {
      val kw = implicitly[KeyWrites[K]]
      val vw = implicitly[Writes[V]]

      def writes(ts: M[K, V]): JsObject =
        JsObject(ts.toSeq.map { case (k, v) =>
          kw.writeKey(k) -> vw.writes(v)
        })
    }

  implicit def keyMapReads[K: KeyReads, V: Reads]: Reads[Map[K, V]] = new Reads[Map[K, V]] {
    val kr = implicitly[KeyReads[K]]
    val vr = implicitly[Reads[V]]

    def reads(json: JsValue): JsResult[Map[K, V]] = json match {
      case JsObject(m) => {
        type Errors = scala.collection.Seq[(JsPath, scala.collection.Seq[JsonValidationError])]
        def locate(e: Errors, key: String) = e.map { case (p, valerr) =>
          (JsPath \ key) ++ p -> valerr
        }

        // !! Keep accumulating the error after the first one
        m.foldLeft(Right(Map.empty): Either[Errors, Map[K, V]]) { case (acc, (key, value)) =>
          val result = for {
            rv <- vr.reads(value)
            rk <- kr.readKey(key)
          } yield rk -> rv

          (acc, result) match {
            case (Right(vs), JsSuccess(v, _)) => Right(vs + v)
            case (Right(_), JsError(e))       => Left(locate(e, key))
            case (Left(e), _: JsSuccess[_])   => Left(e)
            case (Left(e1), JsError(e2))      => Left(e1 ++ locate(e2, key))
          }
        }.fold(JsError.apply(_), res => JsSuccess(res))
      }

      case _ => JsError("error.expected.jsobject")
    }
  }
}
