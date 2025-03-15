package ru.otus.module2

import cats.{Functor, Monoid}
import cats.implicits._
import scala.util.{ Try, Failure, Success }

package object catsHomework {

  /**
   * Простое бинарное дерево
   * @tparam A
   */
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   *
   */
   implicit val treeFunctor: Functor[Tree] = new Functor[Tree]{
     override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
     }
   }

  val t0_1 = Leaf(0)
  val t1_1 = Leaf(1)
  val t2_1 = Branch(
    Branch(
      Branch(
        t0_1, t1_1
      ), Branch(
        t0_1, t1_1
      )
    ), Branch(
      Branch(
        t0_1, t1_1
      ), Branch(
        t0_1, t1_1
      )
    )
  )

  val t0_2 = Leaf("abc")
  val t1_2 = Leaf("xyz")
  val t2_2 = Branch(
    Branch(
      Branch(
        t0_2, t1_2
      ), Branch(
        t0_2, t1_2
      )
    ), Branch(
      Branch(
        t0_2, t1_2
      ), Branch(
        t0_2, t1_2
      )
    )
  )

  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
  }

  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */

   lazy val tryME = new MonadError[Try, Throwable] {

     override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa match {
      case Failure(exception) => raiseError(exception)
      case Success(value) => f(value)
     }

     override def pure[A](v: A): Try[A] = Success(v)

     override def raiseError[A](e: Throwable): Try[A] = Failure(e)

     override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa match {
      case Failure(exception) => f(exception)
      case _ => fa
     }

     override def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa match {
      case Failure(exception) => Success(f(exception))
      case _ => fa
     }

     override def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] = fa match {
      case Failure(exception) => raiseError(exception)
      case Success(value) => if (f(value)) Success(value) else Failure(e)
     }
   }

  /**
   * Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */

   type EitherA[A] = Either[String, A]

   lazy val eitherME = new MonadError[EitherA, String] {

     override def flatMap[A, B](fa: EitherA[A])(f: A => EitherA[B]): EitherA[B] = fa match {
      case Left(exception) => raiseError(exception)
      case Right(value) => f(value)
     }

     override def pure[A](v: A): EitherA[A] = Right(v)

     override def raiseError[A](e: String): EitherA[A] = Left(e)

     override def handleErrorWith[A](fa: EitherA[A])(f: String => EitherA[A]): EitherA[A] = fa match {
      case Left(exception) => f(exception)
      case _ => fa
     }

     override def handleError[A](fa: EitherA[A])(f: String => A): EitherA[A] = fa match {
      case Left(exception) => Right(f(exception))
      case _ => fa
     }

     override def ensure[A](fa: EitherA[A])(e: String)(f: A => Boolean): EitherA[A] = fa match {
      case Left(exception) => raiseError(exception)
      case Right(value) => if (f(value)) Right(value) else Left(e)
     }
   }
}
