package guguguexamples.jsonhttp.client

import cats.effect.IO
import gugugu.lang.scala.runtime.transport._
import guguguexamples.jsonhttp._
import io.circe.Json
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s._

case class HttpClientTransport(http: Client[IO], prefix: Uri)
  extends ClientTransport[WithMeta, WithMeta, IO, Json, Json] {
  override def send[A, B]( name: QualName[String]
                         , fa: WithMeta[A]
                         , encodeA: A => Json, decodeB: Json => B
                         ): IO[WithMeta[B]] = {
    val (reqMeta, a) = fa
    http.fetch {
      for {
        r <- IO.delay(encodeA(a))
      } yield {
        val hs = metaToHeaders(reqMeta)
        val uri = (name.namespace :+ name.name).foldLeft(prefix)(_ / _)
        Request[IO](Method.POST, uri)
          .withEntity(r)
          .putHeaders(hs : _*)
      }
    } { res =>
      for {
        r <- res.as[Json]
        b <- IO.delay(decodeB(r))
      } yield {
        val resMeta = metaFromHeaders(res.headers)
        (resMeta, b)
      }
    }

  }
}
