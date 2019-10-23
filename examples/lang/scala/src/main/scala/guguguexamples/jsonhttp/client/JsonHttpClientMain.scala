package guguguexamples.jsonhttp.client

import cats.effect.{ContextShift, IO, Resource, Timer}
import cats.implicits._
import guguguexamples.codec.JsonCodecImpl
import guguguexamples.definitions.hello._
import guguguexamples.definitions.hellotypes._
import guguguexamples.jsonhttp._
import guguguexamples.utils.EnvConfig
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder

import scala.concurrent.ExecutionContext

object JsonHttpClientMain {

  def main(args: Array[String]): Unit = {
    run.unsafeRunSync()
  }

  def run: IO[Unit] = {
    helloModuleResource.use { hello =>
      for {
        _ <- doRequest {
          FoldRequest(values = Vector(1, 3, 4), initial = 2, op = Operation.Add)
        }(hello.fold)
        _ <- doRequest(10)(hello.calculateFibs)
      } yield ()
    }
  }

  private def doRequest[A, B](a: A)(k: WithMeta[A] => IO[WithMeta[B]]): IO[Unit] = {
    val reqWithMeta = (Map("X-Some-Meta" -> "2333"), a)
    for {
      resWithMeta <- k(reqWithMeta)
      (resMeta, res) = resWithMeta
      _ <- resMeta.toStream.traverse_ { case (k, v) =>
        logger.info(s"Metadata: $k = $v")
      }
      _ <- logger.info(s"Got response: $res")
    } yield ()
  }

  private implicit val contextShiftIO: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  private implicit val timerIO: Timer[IO] = IO.timer(ExecutionContext.global)
  private val logger: Logger[IO] = Slf4jLogger.getLogger

  def helloModuleResource: Resource[IO, HelloModule[WithMeta, WithMeta, IO]] = {
    httpClientResource.map { http =>
      val uriPrefix = Uri(
        scheme = Some(Uri.Scheme.http),
        authority = Some(Uri.Authority(
          host = Uri.RegName(EnvConfig.host),
          port = Some(EnvConfig.port),
        ))
      )
      val t = HttpClientTransport(http, uriPrefix)
      HelloModule.fromTransport(t, JsonCodecImpl, JsonCodecImpl)
    }
  }

  def httpClientResource: Resource[IO, Client[IO]] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
  }

}
