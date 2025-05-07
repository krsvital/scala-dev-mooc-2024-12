package ru.otus.module4.http4s

import cats._
import cats.data._
import cats.effect._
import io.circe.generic.auto._, io.circe.syntax._, io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import com.comcast.ip4s.{Host, Port}

import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration._

import fs2.Stream
import fansi.ErrorMode.Throw

object http4s_homework {
    case class Count(counter: Int)
    
    implicit val encoder: Encoder.AsObject[Count] = deriveEncoder[Count]

    def homeworkService(counter: Ref[IO, Int]): HttpRoutes[IO] = HttpRoutes.of {
        case GET -> Root / "counter" =>
            for {
                value   <- counter.updateAndGet { x => x + 1 }
                res     <- Ok(Count(value).asJson.toString())
            } yield res
        case GET -> Root / "slow" / chunk / total / time =>
            for {
                res <- Try {
                        val chunkInt = chunk.toInt
                        val totalInt = total.toInt
                        val timeInt = time.toInt

                        Stream.unfoldEval(0){ s =>
                            val next = s + chunkInt
                            if (s == -1 || s == totalInt)
                                IO.none
                            else if (next > totalInt)
                                IO.delay(Some(makeChunk(totalInt - s), -1)).delayBy(timeInt.seconds)
                            else
                                if (s == 0)
                                    IO.delay(Some(makeChunk(chunkInt), next))
                                else
                                    IO.delay(Some(makeChunk(chunkInt), next)).delayBy(timeInt.seconds)
                        }
                   } match {
                       case Success(stream) => Ok(stream)
                       case Failure(exception) => BadRequest(exception.getMessage())
                   }
           } yield res
   }

    def makeChunk(size: Int): String =
        (0 to size).foldLeft(List[String]()){
            case (list, _) => "X" :: list
        }.take(size - 1).mkString + "|"

    def Middleware[F[_]: Functor](routes: HttpRoutes[F]): HttpRoutes[F] =
        Kleisli {
            req =>
                routes(req).map {
                    case Status.Successful(resp) => resp.putHeaders("Content-Type"-> "application/json")
                    case response => response
                }
        }

    val server = for {
        count   <- Resource.eval(Ref.of[IO, Int](0))
        s       <- EmberServerBuilder
                    .default[IO]
                    .withPort(Port.fromInt(8998).get)
                    .withHost(Host.fromString("localhost").get)
                    .withHttpApp(Middleware(homeworkService(count)).orNotFound).build
    } yield s
}

object httpService extends IOApp.Simple {
  def run: IO[Unit] = http4s_homework.server.use(_ => IO.never)
}