// applicative
import cats.effect.{Async, Concurrent, IOApp, MonadCancel, Spawn, Temporal}
import cats.effect.unsafe.implicits.global
import cats.{Applicative, MonadError}
import cats.implicits._

import java.lang
import scala.concurrent.duration.DurationInt
/*
val option1: Option[Int] = Some(3)
val option2: Option[Int] = Some(4)

val result: Option[Int] = (option1, option2).mapN(_ + _)

//pure
val optionValue: Option[Int] = Applicative[Option].pure(42)
//ap *> <*
val optionFunc: Option[Int => Int] = Some((x:Int) => x*2)
val optionValue1: Option[Int] = Some(3)

val result1 = optionFunc.ap(optionValue1)


// принцип наименьшей силы
import  cats.Functor

def transform[F[_]: Functor](fa: F[Int]): F[Int] =
  fa.map(_ * 2)

import cats.data.Kleisli
import cats.effect.IO

val getUser: Kleisli[IO, Int, Option[String]] /* OptionT[IO, String]*/ = Kleisli { id =>
  IO.pure(Some("df"))
}
val userName = getUser.run(42)



import cats.Semigroup
import cats.implicits.catsSyntaxParallelSequence_
def combineList[A: Semigroup](a: List[A], b: List[A]): List[A] =
  a |+| b


// MonadError
//MonadError[F[_], E]

object  MonadErrorExample extends  App {
  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadError.pure(42)
  val fail: ErrorOr[Int] = monadError.raiseError("fail")
  val recover: ErrorOr[Int] = monadError.handleError(fail)(_ => 0)

  val io: IO[Int] = IO.raiseError(new RuntimeException("sf"))
  val handle: IO[Int] = io.handleError(_ => 0)


  val result: IO[Either[Throwable, Int]] = io.attempt
}

*/
// MonadCancel
//MonadCancel[F[_], E]
import cats.effect.{IO, MonadCancel}

object TestMonadCancel extends App {
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  val io: IO[Int] = IO.sleep(3.seconds) *> IO.pure(42)
  val cancellable: IO[Int] = io.onCancel(IO.println("cancelled"))
  val program: IO[Unit] = for {
    fiber <- cancellable.start
    _ <- IO.sleep(1.second) *> fiber.cancel
  } yield ()

  program.unsafeRunSync()
}


// clock random unique

import cats.effect.{IO, Clock}
import scala.concurrent.duration._

object TestClock extends App {

  val now: IO[FiniteDuration] = Clock[IO].realTime
  val mono: IO[FiniteDuration] = Clock[IO].monotonic

  val program = for {
    start <- Clock[IO].monotonic
    _ <- IO.sleep(1.second)
    end <- Clock[IO].monotonic
    _ <- IO.println(s"elapsed time : ${end - start}")
  } yield ()
  program.unsafeRunSync()
}


// Sync

//object ReadFile extends IOApp.Simple {
//  def readFile: IO[String] = IO.blocking(scala.io.Source.fromFile("some_file.txt").mkString)
//
//  val program =  for {
//    content <- readFile
//    _ <- IO.println("safd")
//  } yield()
//
//  override def run: IO[Unit] = program
//
//}

// spawn
//import cats.effect.{IO, Spawn}

//val task: IO[Int] = IO.sleep(1.second) *> IO.pure(42)
//
//val program = for {
//  fiber <- task.start
//  result <- fiber.join
//} yield ()
//
//
//val fastTask = IO.sleep(500.millis) *> IO.pure(42)
//val slowTask = IO.sleep(1.second) *> IO.pure(42)
//
//val raceResult = for {
//  result <- fastTask.race(slowTask)
//} yield ()


// Concurrent
//Temporal

//val asynctask: IO[Int] = IO.async_ {callback =>
//  new Thread(new Runnable{
//    def run(): Unit = {
//      callback(Right(52))
//    }
//  }).start()
//}

