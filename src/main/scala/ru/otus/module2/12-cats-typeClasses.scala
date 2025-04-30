package ru.otus.module2


import java.util.Date
import scala.language.postfixOps
import cats.{Contravariant, Functor, Monad, Monoid, Semigroup, Show}
import cats.implicits._

object catsTypeClasses{


//  trait Show[T]{
//    def show(v: T): String
//  }
//
//  object Show{
//    implicit val showInt: Show[Int] = fromToString
//    implicit val showDate: Show[Date] = from(d => s"${d.getTime} ms from the epoch")
//
//    def from[T](f: T => String): Show[T] = t => f(t)
//    def fromToString[T]: Show[T] = _.toString
//
//  }
//
//  object showSyntax{
//    implicit class ShowSyntax[T](v: T){
//      def show(implicit ev: Show[T]): String = ev.show(v)
//    }
//  }
//
//  import showSyntax._
//
//  println(10.show)
//  println(new Date(1000L).show)


  implicit val dateShow = Show.show[Date](d => s"${d.getTime} ms from the epoch")
  println(10.show)
  println(new Date(1000L).show)



  // Semigroup

//  trait Semigroup[T]{
//    def combine(a: T, b: T): T
//  }
//
//  object Semigroup{
//    def apply[A](implicit ev: Semigroup[A]) = ev
//
//    implicit val intSemigroup: Semigroup[Int] = (a, b) => a + b
//  }
//
//  val s1 = Semigroup[Int].combine(2, Semigroup[Int].combine(3, 5))
//  val s2 = Semigroup[Int].combine(Semigroup[Int].combine(2, 3), 5)
//  println(s1)
//  println(s2)

  // Задача мержа 2-х Map

//   val m1 = Map("a" -> 1, "b" -> 2)
//   val m2 = Map("b" -> 3, "c" -> 4)
//   // val m3 = m1 merge m2 // Map("a" -> 1, "b" -> 5, "c" -> 4)
//
//  def optCombine[V : Semigroup](v: V, optV: Option[V]): V =
//    optV.map(v2 => Semigroup[V].combine(v, v2)).getOrElse(v)
//
//  def merge[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
//    lhs.foldLeft(rhs){ case (acc, (k, v)) =>
//      acc.updated(k, optCombine(v, acc.get(k)))
//    }
//
//  println(merge(m1, m2).show)

  // Monoid

//  trait Monoid[T]{
//    def combine(a: T, b: T): T
//    def empty: T
//  }

//  implicit val intMonoid = new Monoid[Int] {
//    override def combine(a: Int, b: Int): Int = a + b
//
//    override def empty: Int = 0
//  }
//
//  object Monoid{
//    def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev
//  }

//  def combineAll[A: Monoid](list: List[A]): A =
//    list.foldLeft(Monoid[A].empty)(Monoid[A].combine)
//
//  println(combineAll(List(2, 2, 3)))

  // Functor

//  trait Functor[F[_]]{
//    def map[A, B](fa: F[A])(f: A => B): F[B]
//  }

//  def doMath[F[_] : Functor](start: F[Int]): F[Int] =
//    start.map(n => n + 1 * 2)
//
//  println(doMath(Option(2)))
//  println(doMath(List(2, 3, 4)))
  // Semigroup

  //  trait Semigroup[T]{
  //    def combine(a: T, b: T): T
  //  }
  //
  //  object Semigroup{
  //    def apply[A](implicit ev: Semigroup[A]) = ev
  //
  //    implicit val intSemigroup: Semigroup[Int] = (a, b) => a + b
  //  }
  //
  //  val s1 = Semigroup[Int].combine(2, Semigroup[Int].combine(3, 5))
  //  val s2 = Semigroup[Int].combine(Semigroup[Int].combine(2, 3), 5)
  //  println(s1)
  //  println(s2)

  // Задача мержа 2-х Map

//  val m1 = Map("a" -> 1, "b" -> 2)
//  val m2 = Map("b" -> 3, "c" -> 4)
  // val m3 = m1 merge m2 // Map("a" -> 1, "b" -> 5, "c" -> 4)

//  def optCombine[V : Semigroup](v: V, optV: Option[V]): V =
//    optV.map(v2 => Semigroup[V].combine(v, v2)).getOrElse(v)
//
//  def merge[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
//    lhs.foldLeft(rhs){ case (acc, (k, v)) =>
//      acc.updated(k, optCombine(v, acc.get(k)))
//    }
//
//  println(merge(m1, m2).show)

  // Monoid

  //  trait Monoid[T]{
  //    def combine(a: T, b: T): T
  //    def empty: T
  //  }

  //  implicit val intMonoid = new Monoid[Int] {
  //    override def combine(a: Int, b: Int): Int = a + b
  //
  //    override def empty: Int = 0
  //  }
  //
  //  object Monoid{
  //    def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev
  //  }

  def combineAll[A: Monoid](list: List[A]): A =
    list.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  println(combineAll(List(2, 2, 3)))

  // Functor

  //  trait Functor[F[_]]{
  //    def map[A, B](fa: F[A])(f: A => B): F[B]
  //  }

  def doMath[F[_] : Functor](start: F[Int]): F[Int] =
    start.map(n => n + 1 * 2)

  println(doMath(Option(2)))
  println(doMath(List(2, 3, 4)))
  val f1: Int => String = i => (i * 2).toString
  val f2: String => Unit = println

  val f3: Int => Unit = f1 map f2

  f3(10)

  // Contravariant
  // contramap(f: B => A): F[B]

  class Id(val raw: String)
  class User(val id: Id)

  val id = new Id("10")
  val user = new User(id)


  implicit val showId: Show[Id] = Show.show(id => s"Id: ${id.raw}")
  implicit val showUser: Show[User] = showId.contramap(_.id)

  println(id.show)
  println(user.show)

  // Invariant

  implicit val semiGroupDate: Semigroup[Date] =
    Semigroup[Long].imap(new Date(_))(_.getTime)

  val d1 = new Date(1000L)
  val d2 = new Date(1000l)

  val d3 = d1 |+| d2

  println(d1.show)
  println(d2.show)
  println(d3.show)

  // Monad

//  trait Monad[F[_]]{
//    def pure[A](v: A): F[A]
//    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
//
//    def map[A, B](fa: F[A])(f: A => B): F[B] =
//      flatMap(fa)(a => pure(f(a)))
//  }

  val o1: Option[Int] = Monad[Option].pure(3)
  val o2: Option[Int] = Monad[Option].flatMap(o1)(i => Some(i + 2))


}