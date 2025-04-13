package ru.otus.module3

import scala.language.postfixOps
import zio._
import java.util.concurrent.TimeUnit
import ru.otus.module3.zio_homework.config.Configuration
import ru.otus.module3.zio_homework.config.AppConfig

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  val readStdin: zio.ZIO[Any, Throwable, Int] = for {
    raw <- zio.Console.readLine("[1-3]")
    res <- ZIO.attempt {
      val regex = """([1-3])""".r
      val exit = """(exit)""".r
      raw match {
        case regex(value) => value.toInt
        case exit(_) => -1
        case _ => -2
      }
    }
    int <- if (res == -2) zio.Console.printLine("Input format error, please try again") *> readStdin else ZIO.succeed(res)
  } yield int

  lazy val guessProgram = 
    for {
      _ <- zio.Console.printLine("Guess the number from 1 to 3 or write 'exit' to quit")
      rnd <- zio.Random.nextIntBetween(1, 4)
      number <- readStdin
      _ <- if (number == -1) zio.Console.printLine("quit") else if (rnd == number) zio.Console.printLine("You Win!") else zio.Console.printLine(s"You didn't guess, the correct result was ${rnd}!")
    } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  implicit class Reapeated[-R, +E, +A](z: ZIO[R, E, A]){
    def doWhile(f: A => Boolean): ZIO[R, E, A] = z.flatMap{ a => 
      if(f(a)) z else doWhile(f)
    }
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "Configuration.config" из пакета config
   */

  def loadConfigOrDefault = 
    Configuration.config.orElse(
        ZIO.succeed(
          AppConfig(
            host = "127.0.0.1",
            port = "8080"
        )
      )
    )

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = Random.nextIntBetween(0, 11).delay(1.second)
  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = (0 to 9).foldLeft(List.empty[ZIO[Any,Nothing,Int]]){
    case (list, _) => eff :: list
  }

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  implicit class RunningTime[-R, +E >: Throwable, +A](zio: ZIO[R, E, A]){
    def printEffectRunningTime: ZIO[R, E, A] = for {
      start <- Clock.currentTime(TimeUnit.MILLISECONDS)
      effect <- zio
      end <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _ <- Console.printLine(s"Running time: ${end - start}")
    } yield effect
  }

  lazy val app = for { 
    list <- ZIO.foreach(effects){ z => z }.printEffectRunningTime
    _ <- Console.printLine(list)
  } yield list.sum


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = for { 
    list <- ZIO.foreachPar(effects){ z => z }.printEffectRunningTime
    _ <- Console.printLine(list)
  } yield list.sum

  lazy val appSpeedUpNoTime = for { 
    list <- ZIO.foreachPar(effects){ z => z }
    _ <- Console.printLine(list)
  } yield list.sum


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */

  trait EffectRunningTime {
    def printEffectRunningTime[R, E >: Throwable, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = for {
      start <- Clock.currentTime(TimeUnit.MILLISECONDS)
      effect <- zio
      end <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _ <- Console.printLine(s"Running time: ${end - start}")
    } yield effect
  }

   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg = for {
    rt <- ZIO.service[EffectRunningTime]
    sum <- rt.printEffectRunningTime(appSpeedUpNoTime)
    _ <- Console.printLine(sum)
  } yield ()

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp = appWithTimeLogg.provide(
    ZLayer(ZIO.succeed(new EffectRunningTime {}))
  )
}
