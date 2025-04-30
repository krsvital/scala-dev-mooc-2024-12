package ru.otus.module3

import ru.otus.module3.tryFinally.zioResource
import ru.otus.module3.tryFinally.zioResource.{closeDummyFile, closeFile, handleFile, openDummyFile, openFile}
import zio.{CancelableFuture, Console, IO, RIO, Scope, Task, UIO, URIO, ZIO}

import java.io.{BufferedReader, Closeable, File, FileReader, IOException}
import scala.concurrent.impl.Promise
import scala.concurrent.{Future, Promise, blocking}
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import scala.language.postfixOps
import scala.io.BufferedSource

object tryFinally {

  object traditional {


    def acquireResource: Resource = Resource("Some resource")

    def use(r: Resource): Unit = println(s"Using resource: ${r.name}")

    def releaseResource(r: Resource): Unit  = r.close()

    /**
     * Напишите код, который обеспечит корректную работу с ресурсом:
     * получить ресурс -> использовать -> освободить
     *
     */

    lazy val result = {
      val r = acquireResource
      try{
        use(r)
      } finally {
        releaseResource(r)
      }
    }

    /**
     *
     * обобщенная версия работы с ресурсом
     */



    def withResource[R, A](resource: => R)(release: R => Any)(use: R => A): A = {
      val r = resource
      try{
        use(r)
      } finally {
        release(r)
      }
    }



    /**
     * Прочитать строки из файла
     */



  }

  object future{
    implicit val global = scala.concurrent.ExecutionContext.global

    def acquireFutureResource = Future(Resource("Future resource"))

    def use(resource: Resource) = Future(traditional.use(resource))

    def releaseFutureResource(resource: Resource) =
      Future(traditional.releaseResource(resource))

    /**
     * Написать вспомогательный оператор ensuring, который позволит корректно работать
     * с ресурсами в контексте Future
     *
     */

     implicit class FutureOps[A](future: Future[A]){
      def ensuring(finalizer: Future[Any]): Future[A] =
        future.transformWith{
          case Failure(exception) =>
            finalizer.flatMap(_ => Future.failed(exception))
          case Success(value) =>
            finalizer.flatMap(_ => Future.successful(value))
        }
    }



    /**
     * Написать код, который получит ресурс, воспользуется им и освободит
     */

    val futureResult: Future[Unit] =
      acquireFutureResource.flatMap(r => use(r)
      .ensuring(releaseFutureResource(r)))



  }

  object zioResource{


    /**
     * реализовать ф-цию, которая будет описывать открытие файла с помощью ZIO эффекта
     */
    def openFile(fileName: String) =
      ZIO.attempt(Source.fromFile(fileName))

    def openDummyFile(fileName: String) =
      ZIO.attempt(Resource(fileName))
    /**
     * реализовать ф-цию, которая будет описывать закрытие файла с помощью ZIO эффекта
     */

    def closeFile(file: Source) = ZIO.attempt(file.close()).orDie

    def closeDummyFile(file: Resource) = ZIO.attempt(file.close()).orDie

    /**
     * Написать эффект, который прочитает строчки из файла и выведет их в консоль
     */

    def handleFile(file: Source) = ZIO.foreach(file.getLines().toList){ str =>
      zio.Console.printLine(str)
    }

    def handleDummyFile(file: Resource) =
      zio.Console.printLine(s"Using resource: ${file.name}")


    val r1: Task[Unit] = ZIO.acquireReleaseWith(openFile("test.txt"))(closeFile){ f =>
      handleFile(f).unit
    }

    val r2: Task[Unit] = ZIO.acquireReleaseWith(openDummyFile("Dummy file"))(closeDummyFile){ f =>
      handleDummyFile(f)
    }

    /**
     * Написать эффект, который откроет 2 файла, прочитает из них строчки,
     * выведет их в консоль и корректно закроет оба файла
     */

    val r3 = ZIO.acquireReleaseWith(openDummyFile("Dummy file 1"))(closeDummyFile){ f1 =>
      ZIO.acquireReleaseWith(openDummyFile("Dummy file 2"))(closeDummyFile){ f2 =>
        handleDummyFile(f1) zipRight handleDummyFile(f2)
      }
    }



    /**
     * Рефакторинг выше написанного кода
     *
     */

    def withFile = ???


    lazy val twoFiles2 = ???

  }

}


object zioScope{



  /**
   * написать эффект открывающий / закрывающий первый файл
   */
  lazy val file1: ZIO[Any with Scope, Throwable, BufferedSource] =
    ZIO.acquireRelease(zioResource.openFile("test1.txt"))(zioResource.closeFile)

  /** написать эффект открывающий / закрывающий второй файл
    *
   */
  lazy val file2 =
    ZIO.acquireRelease(zioResource.openFile("test2.txt"))(zioResource.closeFile)


  /**
   * Использование ресурсов
   */

  val fileCombined: ZIO[Any with Scope, Throwable, (BufferedSource, BufferedSource)] = file1 zip file2

  /**
   * Написать эффект, который воспользуется ф-ей handleFile из блока про bracket
   * для печати строчек в консоль
   */

   val r1: ZIO[Any, Throwable, Unit] = ZIO.scoped{
     fileCombined.flatMap{ case (f1, f2) =>
       (handleFile(f1) zipRight handleFile(f2)).unit
     }
   }

   val fileDummy1: ZIO[Any with Scope, Throwable, Resource] = ZIO.acquireRelease(openDummyFile("Scope dummy 1"))(closeDummyFile)
   val fileDummy2: ZIO[Any with Scope, Throwable, Resource] = ZIO.acquireRelease(openDummyFile("Scope dummy 2"))(closeDummyFile)

  val combinedDummyFile = fileDummy1 zipPar fileDummy2

  val r1Dummy = ZIO.scoped{
    combinedDummyFile.flatMap{ case (f1, f2) =>
      zio.Console.printLine(f1.name) zipRight zio.Console.printLine(f2.name)
    }
  }

  /**
   * Комбинирование ресурсов
   */



  // Комбинирование


  /**
   * Написать эффект, который прочитает и выведет строчки из обоих файлов
   */





  /**
   * Множество ресурсов
   */

  lazy val fileNames: List[String] = List(
    "Scope R1",
    "Scope R2",
    "Scope R3",
    "Scope R4",
    "Scope R5",
    "Scope R6",
    "Scope R7",
    "Scope R8",
    "Scope R9",
    "Scope R10"
  )

  def file(name: String) =
    ZIO.acquireRelease(openDummyFile(name))(closeDummyFile)


  // множественное открытие / закрытие
  lazy val files = ZIO.foreachPar(fileNames){ n =>
    file(n)
  }

  val cc = ZIO.scoped{
    files.flatMap{ list =>
      ZIO.foreach(list){ f =>
        zio.Console.printLine(f.name)
      }
    }
  }


  // параллельное множественное открытие / закрытие
  lazy val files2 = ???


  // Использование


  // обработать N файлов



  lazy val files3: ZIO[Any with Scope, IOException, List[Source]] = ???

  /**
   * Прочитать строчки из файлов и вернуть список этих строк используя files3
   */
  lazy val r3: Task[List[String]] = ???
  


  val eff1: Task[BufferedSource] = ZIO.attempt(Source.fromFile("test.txt"))

  val cc2: ZIO[Any with Scope, Throwable, BufferedSource] =
    eff1.withFinalizer(s => ZIO.attempt(s.close()).orDie)

  val cc3: ZIO[Any with Scope, Throwable, BufferedSource] =
    ZIO.fromAutoCloseable(eff1)

  val cc4: ZIO[Any with Scope, Throwable, BufferedSource] =
    eff1.withFinalizerAuto


  type Transactor

  def mkTransactor(c: Config): ZIO[Any with Scope, Throwable, Transactor] = ???

  type Config
  lazy val config: Task[Config] = ???

  lazy val m2 = ???

}