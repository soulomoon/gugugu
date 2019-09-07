package guguguexamples.jsonhttp.server.impl

import cats.effect.{IO, Timer}
import cats.implicits._
import guguguexamples.definitions.hello._
import guguguexamples.jsonhttp.WithMeta
import guguguexamples.jsonhttp.server.HandlerF
import guguguexamples.utils.ContT
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.MILLISECONDS

class HelloImpl(implicit timer: Timer[IO])
  extends HelloModule[WithMeta, WithMeta, HandlerF] {

  private val logger: Logger[IO] = Slf4jLogger.getLogger

  override def fold(fa: WithMeta[FoldRequest]): HandlerF[WithMeta[Int]] = {
    withMeta(fa) { req =>
      ContT.lift {
        IO.pure {
          req.values.foldLeft(req.initial)(_ + _)
        }
      }
    }
  }

  private def withMeta[A, B](fa: WithMeta[A])
                            (k: A => HandlerF[B]): HandlerF[WithMeta[B]] = {
    val (metadata, a) = fa
    for {
      begin <- ContT.lift(timer.clock.realTime(MILLISECONDS))
      _ <- metadata.toStream.traverse_[HandlerF, Unit] { case (k, v) =>
        ContT.lift(logger.info(s"Got Metadata: $k = $v"))
      }
      b <- k(a)
      end <- ContT.lift(timer.clock.realTime(MILLISECONDS))
    } yield (Map("X-Process-Time" -> s"${end - begin}ms"), b)
  }

}
