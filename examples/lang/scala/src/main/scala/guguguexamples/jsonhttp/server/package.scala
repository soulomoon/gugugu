package guguguexamples.jsonhttp

import cats.effect.IO
import guguguexamples.utils.ContT
import org.http4s.Response

package object server {
  type HandlerF[A] = ContT[Response[IO], IO, A]
}
