package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.Monad
import cats.effect.kernel.{ Ref, Sync }
import cats.effect.{IO, IOApp}
import cats.implicits._
import Wallet.{BalanceTooLow, WalletError}
import ru.otus.module3.catsconcurrency.cats_effect_homework.Wallet.WalletException

// Здесь мы хотим протестировать бизнес-логику использующую кошельки: функцию transfer.
// Однако мы не хотим в наших тестах создавать какие-то файлы: в реальном приложении такой тест будет нуждаться в базе данных,
// что очень неудобно, ведь мы тестируем не сами кошельки а функцию transfer.

// Для таких тестов нам бы очень пригодился in-memory кошелёк, который хранит свой баланс в памяти пока работает тест.
// Он будет очень быстрым и дешевым - то что нужно для тестирования.

// Для такой задачи хорошо подойдет cats.effect.kernel.Ref - ячейка памяти с атомарным доступом.
// Нужно сделать интерпретатор Wallet[IO] который будет использовать Ref и запустить тест с помощью такого Wallet.
object WalletTransferApp extends IOApp.Simple {

  // функция, которую мы тестируем. Здесь менять ничего не нужно :)
  def transfer[F[_]: Monad](a: Wallet[F],
                            b: Wallet[F],
                            amount: BigDecimal): F[Unit] =
    a.withdraw(amount).flatMap {
      case Left(_)  => a.topup(amount)
      case Right(_) => b.topup(amount)
    }

  // todo: реализовать интерпретатор (не забывая про ошибку списания при недостаточных средствах)
  final class InMemWallet[F[_] : Sync](ref: Ref[F, BigDecimal]) extends Wallet[F] {
    def balance: F[BigDecimal] = ref.get
    def topup(amount: BigDecimal): F[Unit] = ref.update { value => value + amount }
    def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
      cur   <- ref.get
      value <- if (cur < amount)
                  Sync[F].delay(Left(BalanceTooLow))
               else
                  ref.update { v => cur - amount }.as(Right(()))
    } yield value
    def getId: Wallet.WalletId = "undefined"
  }

  // todo: реализовать конструктор. Снова хитрая сигнатура, потому что создание Ref - это побочный эффект
  def wallet(balance: BigDecimal): IO[Wallet[IO]] = for {
    ref <- Ref.of[IO, BigDecimal](balance)
  } yield new InMemWallet(ref)

  // а это тест, который выполняет перевод с одного кошелька на другой и выводит балансы после операции. Тоже менять не нужно
  def testTransfer: IO[(BigDecimal, BigDecimal)] =
    for {
      w1 <- wallet(100)
      w2 <- wallet(200)
      _ <- transfer(w1, w2, 50)
      b1 <- w1.balance
      b2 <- w2.balance
    } yield (b1, b2)

  def run: IO[Unit] = {
    // 50, 250
    testTransfer.flatMap { case (b1, b2) => IO.println(s"$b1,$b2") }
  }

}