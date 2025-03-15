package ru.otus

import ru.otus.module1.{future, hof, threads, type_system}
import ru.otus.module2.{catsTypeClasses, dataStructures, functional, transformers, validation}
import ru.otus.module2.implicits.{implicit_conversions, implicit_scopes}
import ru.otus.module3.functional_effects.functionalProgram

import scala.util.{Failure, Success}

import ru.otus.module2.catsHomework._
import cats._
import cats.implicits._

object Main {

  def main(args: Array[String]): Unit = {

    val result_1 = Functor[Tree].map(t2_1){ item =>
      s"Item to string: ${item}"
    }

    println(result_1)

    val result_2 = Functor[Tree].map(t2_2){ item =>
      s"String as IntArray: ${item.toCharArray().map(_.toInt).mkString(",")}"
    }

    println(result_2)


//      println(s"Hello " +
//        s"from ${Thread.currentThread().getName}")
//      val t0 = new threads.Thread1
//      val t1 = new Thread{
//        override def run(): Unit = {
//          Thread.sleep(1000)
//          println(s"Hello " +
//            s"from: ${Thread.currentThread().getName}")
//        }
//      }
//      t1.start()
//      t1.join()
//      t0.start()

//    def rates = {
//      val t1 = future.getRatesLocation1
//      val t2 = future.getRatesLocation2
//      t1.flatMap{ i1 =>
//        t2.map{ i2 =>
//          i1 + i2
//        }(future.ec1)
//      }(future.ec1)


//      t1.onComplete {
//        case Failure(exception) =>
//          println(exception.getMessage)
//        case Success(i1) =>
//          t2.onComplete {
//            case Failure(exception) =>
//              println(exception.getMessage)
//            case Success(i2) =>
//              println(s"Sum ${i1 + i2}")
//          }
//      }

//     val sum: threads.ToyFuture[Int] = for{
//        i1 <- t1
//        i2 <- t2
//      } yield i1 + i2
//
//     sum.onComplete(println)
    //   }


  //  catsTypeClasses
    // Thread.sleep(4000)
//     functionalProgram.declarativeEncoding.interpret(
//       functionalProgram.declarativeEncoding.greet)



  }
}
