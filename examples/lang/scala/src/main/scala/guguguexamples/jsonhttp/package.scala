package guguguexamples

import org.http4s.{Header, Headers}

import scala.language.higherKinds

package object jsonhttp {

  type WithMeta[A] = (Map[String, String], A)

  def metaFromHeaders(headers: Headers): Map[String, String] = {
    headers.foldLeft(Map.empty[String, String]) { (map, h) =>
      map + ((h.name.value, h.value))
    }
  }

  def metaToHeaders(metadata: Map[String, String]): Vector[Header] = {
    metadata.map { case (k, v) =>
      Header(k, v)
    }.toVector
  }

}
