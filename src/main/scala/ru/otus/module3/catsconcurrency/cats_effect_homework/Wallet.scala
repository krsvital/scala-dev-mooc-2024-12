package ru.otus.module3.catsconcurrency.cats_effect_homework

import java.nio.file.Files._

import cats.effect.Sync
import cats.implicits._
import Wallet._
import java.nio.file.Files
import java.nio.file.Path
import scala.util.{ Try, Success, Failure }

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
  // вывод имени кошелька
  def getId: WalletId
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {
  private val walletPath = Path.of(s"/tmp/${id}")
  def balance: F[BigDecimal] = for {
    value <- Sync[F].delay {
      if(Files.exists(walletPath))
        Try {
          BigDecimal(Files.readString(walletPath))
        } match {
          case Failure(exception) => BigDecimal(0)
          case Success(value) => value
        }
      else
        BigDecimal(0)
      }
  } yield value
  def topup(amount: BigDecimal): F[Unit] = for {
    balance <- balance
    _       <- write(balance + amount)
  } yield ()
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    cur     <- Sync[F].defer(balance)
    value   <- if(amount > cur)
                  Sync[F].delay(Left(BalanceTooLow))
                else
                  Sync[F].defer(write(cur - amount)) *> Sync[F].delay(Right())
  } yield value

  def getId: WalletId = id

  private def write(amount: BigDecimal): F[Unit] =
    Try {
        Files.write(walletPath, (amount).toString().getBytes())
    } match {
        case Failure(exception) => Sync[F].raiseError(WalletException(exception.getMessage()))
        case Success(_) => Sync[F].delay(())
    }
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = Sync[F].delay(new FileWallet(id))

  // Не нашёл никаких сайд эффектов при создании смарт конструктора, так как реальных действий с файлами нет,
  // создаётся только экземпляр класса. Если кошелек с таким же id уже существует, то его баланс берётся из файла,
  // если же не существует, то он просто равен 0, это описано в методе balance, по сути мы ловим только ошибки I/O
  // в методе write + проверяем наличие средств в методе withdraw

  type WalletId = String

  sealed trait WalletError extends Throwable
  case object BalanceTooLow extends WalletError
  case class WalletException(error: String) extends  WalletError
}
