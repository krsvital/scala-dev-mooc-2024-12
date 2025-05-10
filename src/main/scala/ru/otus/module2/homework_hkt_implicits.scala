package ru.otus.module2

object homework_hkt_implicits{
  trait Bind[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  object Bind {
    def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit bind: Bind[F]): F[(A, B)] = bind.flatMap(fa){a => bind.map(fb)((a, _))}

    implicit def listToBind: Bind[List] = new Bind[List] {
       override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
       override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

    implicit def optionToBind: Bind[Option] = new Bind[Option] {
        override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
        override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }
  }
}