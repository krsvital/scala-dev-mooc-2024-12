package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import scala.concurrent.duration._
import cats.effect.kernel.Fiber

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())

object WalletFibersApp extends IOApp.Simple {
  def run: IO[Unit] =
    for {
      _ <- IO.println("Press 'exit' to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      fiber1  <- process(wallet1.topup(100), 100.milliseconds).start
      fiber2  <- process(wallet2.topup(100), 500.milliseconds).start
      fiber3  <- process(wallet3.topup(100), 2000.milliseconds).start
      fiber4  <- process(printBalance(wallet1, wallet2, wallet3), 1000.milliseconds).start
      _       <- process(readStdIn(fiber1, fiber2, fiber3, fiber4), 0.milliseconds).handleError{ e => println(e.getMessage()) }
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
    } yield ()

    def process(computation: IO[Unit], delay: FiniteDuration): IO[Unit] =
      computation.flatMap(_ => process(computation, delay).delayBy(delay))
    
    def readStdIn(fiber: Fiber[IO,Throwable,Unit]*): IO[Unit] =
      IO.readLine.flatMap { line =>
        line match {
          case "exit" => fiber.forallM[IO] { f => f.cancel.as[Boolean](true) } *> IO.raiseError(new Throwable("Process stoped"))
          case _ => IO.unit
        }
      }

    def printBalance(wallet: Wallet[IO]*): IO[Unit] = IO.unit.flatMap{ _ => wallet.forallM[IO] { w =>
      for {
          cur <- w.balance
          _   <- IO.println(s"${w.getId} => ${cur}")
        } yield true
      } *> IO.unit
    }
}