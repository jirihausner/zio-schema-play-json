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
}
