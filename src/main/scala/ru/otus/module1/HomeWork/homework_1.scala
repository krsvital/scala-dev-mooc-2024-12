package ru.otus.module1.HomeWork

import scala.annotation.tailrec

object homework_1 {
    trait List[+T]{
        def ::[TT >: T](elem: TT): List[TT] = new ::(elem, this)

        def :+[TT >: T](list: List[TT]): List[TT] = {
            @tailrec
            def loop[TT >: T](list: List[TT], res: List[TT] = this): List[TT] = {
                list match {
                    case Nil => res
                    case head :: tail => loop(tail, new ::(head, res))
                }
            }

            loop(list)
        }

        def reverse: List[T] = {
            @tailrec
            def loop[TT >: T](list: List[TT] = this, res: List[TT] = Nil): List[TT] = {
                list match {
                    case Nil => res
                    case head :: tail => loop(tail, new ::(head, res))
                }
            }

            loop()
        }

        def map[R](f: T => R): List[R] = flatMap(v => List(f(v)))

        def flatMap[R](f: T => List[R]): List[R] = {
            @tailrec
            def loop(list: List[T] = this, res: List[R] = Nil): List[R] = {
                list match {
                    case Nil => res
                    case head :: tail => loop(tail,  res :+ f(head))
                }
            }

            loop().reverse
        }

        def filter(f: T => Boolean): List[T] = {
            @tailrec
            def loop(list: List[T] = this, res: List[T] = Nil): List[T] = {
                list match {
                    case Nil => res
                    case head :: tail if f(head) => loop(tail, new ::(head, res))
                    case head :: tail => loop(tail, res)
                }
            }

            loop().reverse
        }

        def mkString(delim: String = " "): String = {
            def loop(list: List[T] = this, res: String = null): String = {
                list match {
                    case Nil => res
                    case head :: tail if res == null => loop(tail, s"${head}")
                    case head :: tail => loop(tail, s"${res}${delim}${head.toString()}")
                }
            }
            
            loop()
        }

        def mkString: String = mkString()
    }

    case class ::[T](head: T, tail: List[T]) extends List[T]
    case object Nil extends List[Nothing]

    object List{
        def apply[A](v: A*): List[A] = if(v.isEmpty) Nil
        else new ::(v.head, apply(v.tail:_*))

        def incList(list: List[Int]): List[Int] = list.map(x => x + 1)
        def shoutString(list: List[String]): List[String] = list.map(x => "!" + x)
    }
}

