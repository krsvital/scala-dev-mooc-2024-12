package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.Promise
import scala.util.{ Failure, Success, Try }

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val promise = Promise[(List[A], List[Throwable])]()

    def loop(promises: List[Future[Try[A]]], res: (List[A], List[Throwable]) = (List[A](), List[Throwable]())): Unit = {
      promises match {
        case Nil => promise.success(res)
        case head :: next => head.isCompleted match {
          case true => head.value.get.get match {
            case Failure(exception) => loop(next, (res._1, exception :: res._2))
            case Success(value) => loop(next, (value :: res._1, res._2))
          }
          case false => loop(promises, res)
        }
      }
    }

    Future(
      loop(
        futures.foldLeft(List[Future[Try[A]]]()){
          case (list, f) => {
            val p = Promise[Try[A]]()
            f.onComplete { fp =>
              p.success(fp)
            }
            p.future :: list
          }
        }
      )
    )

    promise.future
  }

}
