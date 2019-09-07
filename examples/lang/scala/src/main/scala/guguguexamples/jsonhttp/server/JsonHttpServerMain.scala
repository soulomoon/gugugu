package guguguexamples.jsonhttp.server

import java.net.InetSocketAddress

import cats.data.{Kleisli, NonEmptyVector}
import cats.effect.{ContextShift, IO, Resource, Timer}
import gugugu.lang.scala.runtime.transport._
import guguguexamples.codec.JsonCodecImpl
import guguguexamples.definitions.hello._
import guguguexamples.jsonhttp._
import guguguexamples.jsonhttp.server.impl._
import guguguexamples.utils._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.Json
import org.http4s._
import org.http4s.circe._
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

object JsonHttpServerMain {

  def main(args: Array[String]): Unit = {
    run.unsafeRunSync()
  }

  def run: IO[Unit] = {
    serverResource.use { _ =>
      IO.delay {
        println("Press Ctrl-D to shut down the server")
        while (System.in.read() >= 0) {}
        println("Shutting down the server")
      }
    }
  }

  private implicit val contextShiftIO: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private implicit val timerIO: Timer[IO] = IO.timer(ExecutionContext.global)
  private val logger: Logger[IO] = Slf4jLogger.getLogger

  def serverResource: Resource[IO, Server[IO]] = {
    val addr = InetSocketAddress.createUnresolved(
      EnvConfig.host, EnvConfig.port)
    BlazeServerBuilder[IO]
      .bindSocketAddress(addr)
      .withHttpApp {
        Kleisli { req =>
          for {
            _ <- logger.info(s"Got request: $req")
            r <- handleRequest(req).run(IO.pure)
          } yield r
        }
      }
      .resource
  }

  lazy val transport: ServerTransport[WithMeta, WithMeta, HandlerF, Json, Json] = {
    HelloModule.toTransport(new HelloImpl, JsonCodecImpl, JsonCodecImpl)
  }

  def handleRequest(req: Request[IO]): HandlerF[Response[IO]] = {
    (for {
      ps <- NonEmptyVector.fromVector {
        req.pathInfo.stripPrefix("/").split('/').toVector
      }
      qualName = QualName(ps.init, ps.last)
      k <- transport.ask(qualName, ContCodecHandler)
    } yield {
      val reqMeta = metaFromHeaders(req.headers)
      for {
        reqR <- ContT.lift {
          req.as[Json].attempt
        }.flatMap {
          case Right(v) => ContT.pure[Response[IO], IO, Json](v)
          case Left(e) => ContT.completeWith[Response[IO], IO, Json] {
            IO.pure {
              Response[IO](Status.BadRequest)
            }
          }
        }
        resWithMeta <- k((reqMeta, reqR))
      } yield {
        val (resMeta, resR) = resWithMeta
        val hs = metaToHeaders(resMeta)
        Response[IO]()
          .withEntity(resR)
          .putHeaders(hs: _*)
      }
    }).getOrElse(ContT.pure(Response[IO](Status.NotFound)))

  }

}
